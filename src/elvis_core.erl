-module(elvis_core).

-feature(maybe_expr, enable).

%% Public API

-export([rock/1]).
-export([start/0]).
%% for internal use only
-export([do_rock/2]).
%% for eating our own dogfood
-export([main/1]).

-export_type([target/0]).

-ifdef(TEST).

-export([apply_rule/2]).
% For tests (we can't Xref the tests because rebar3 fails to compile some files).
-ignore_xref([apply_rule/2]).

-endif.

% For eating our own dogfood.
-ignore_xref([main/1]).
% For internal use only
-ignore_xref([do_rock/2]).
% For shell usage.
-ignore_xref([start/0]).
% API exports, not consumed locally.
-ignore_xref([rock/1]).

-type source_filename() :: nonempty_string().
-type target() :: source_filename() | module().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(elvis_core),
    ok.

%% In this context, `throw` means an error, e.g., validation or internal, not an actual
%% call to `erlang:throw/1`.
-spec rock([elvis_config:t()]) -> ok | {errors, _} | {warnings, _}.
rock(ElvisConfig) ->
    maybe
        ok ?= elvis_config:validate_config(ElvisConfig),
        _ = elvis_ruleset:drop_custom(),
        Results = lists:map(fun do_parallel_rock/1, ElvisConfig),
        ok ?= lists:foldl(fun combine_results/2, ok, Results)
    else
        {error, Term} ->
            {errors_or_warnings(), Term}
    end.

errors_or_warnings() ->
    case elvis_config:warnings_as_errors() of
        true ->
            errors;
        false ->
            warnings
    end.

%% In this context, `throw` means an error, e.g., validation or internal, not an actual
%% call to `erlang:throw/1`.
-spec do_parallel_rock(elvis_config:t()) ->
    ok
    | {error, [elvis_result:file() | elvis_result:rule()]}.
do_parallel_rock(ElvisConfig0) ->
    Parallel = elvis_config:parallel(),
    ElvisConfig = elvis_config:resolve_files(ElvisConfig0),
    Files = elvis_config:files(ElvisConfig),

    Result =
        elvis_task:chunk_fold(
            {?MODULE, do_rock},
            fun(Elem, Acc) ->
                elvis_result:print_results(Elem),
                {ok, [Elem | Acc]}
            end,
            [],
            [ElvisConfig],
            Files,
            Parallel
        ),
    case Result of
        {ok, Results} ->
            elvis_result_status(Results);
        {error, _} = Error ->
            Error
    end.

-spec do_rock(elvis_file:t(), [elvis_config:t()] | elvis_config:t()) ->
    {ok, elvis_result:file()}.
do_rock(File, ElvisConfig) ->
    maybe
        {ok, LoadedFile} ?= load_file_data(ElvisConfig, File),
        Results = apply_rules(ElvisConfig, LoadedFile),
        {ok, Results}
    else
        {error, _} = Error ->
            Error
    end.

-spec load_file_data([elvis_config:t()] | elvis_config:t(), elvis_file:t()) ->
    {ok, elvis_file:t()} | {error, string()}.
load_file_data(ElvisConfig, File) ->
    Path = elvis_file:path(File),
    _ = elvis_utils:info("Loading ~s", [Path]),
    try
        {ok, elvis_file:load_file_data(ElvisConfig, File)}
    catch
        _:Reason ->
            Msg = "~w when loading file ~p.",
            {error, elvis_utils:error(Msg, [Reason, Path])}
    end.

-spec main([]) -> true | no_return().
main([]) ->
    ok = application:load(elvis_core),
    {module, _} = code:ensure_loaded(elvis_style),
    case rock(elvis_config:config()) of
        ok -> true;
        _ -> elvis_utils:erlang_halt(1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec combine_results(
    ok | {error, [elvis_result:file()]},
    ok | {error, [elvis_result:file()]}
) ->
    ok | {error, [elvis_result:file()]}.
combine_results(ok, Acc) ->
    Acc;
combine_results(Item, ok) ->
    Item;
combine_results({error, ItemResults}, {error, AccResults}) ->
    {error, ItemResults ++ AccResults}.

-spec apply_rules(
    [elvis_config:t()] | elvis_config:t(),
    File :: elvis_file:t()
) ->
    elvis_result:file().
apply_rules(ElvisConfig, File) ->
    Rules = elvis_config:rules(ElvisConfig),
    Acc = {[], ElvisConfig, File},
    {ParseTree, _} = elvis_file:parse_tree(File, ElvisConfig),
    {RulesResults, _, _} =
        lists:foldl(fun apply_rule/2, Acc, merge_rules({file, ParseTree}, lists:flatten(Rules))),
    elvis_result:new(file, File, RulesResults).

merge_rules({file, ParseTree}, ElvisConfigRules) ->
    {nodes, ElvisAttrs} =
        elvis_code:find(#{
            of_types => [elvis],
            inside => ParseTree
        }),
    ElvisAttrRules = elvis_attr_rules(ElvisAttrs),
    elvis_config:merge_rules(ElvisAttrRules, ElvisConfigRules).

elvis_attr_rules([] = _ElvisAttrs) ->
    [];
elvis_attr_rules(ElvisAttrs) ->
    [Rule || ElvisAttr <- ElvisAttrs, Rule <- ktn_code:attr(value, ElvisAttr)].

-spec apply_rule(Rule, {Results, ElvisConfig, File}) -> Result when
    Rule :: elvis_rule:t(),
    Results :: [elvis_result:rule() | elvis_result:elvis_error()],
    ElvisConfig :: elvis_config:t(),
    File :: elvis_file:t(),
    Result :: {Results, ElvisConfig, File}.
apply_rule(Rule, {Result, ElvisConfig, File}) ->
    RuleResult =
        try
            AnalyzedModule = elvis_file:module(File),
            IgnorableModule = elvis_rule:ignorable(AnalyzedModule),
            case elvis_rule:ignored(IgnorableModule, Rule) of
                false ->
                    Results = elvis_rule:execute(elvis_rule:file(Rule, File), ElvisConfig),
                    SortFun = fun(#{line_num := L1}, #{line_num := L2}) -> L1 =< L2 end,
                    SortResults = lists:sort(SortFun, Results),
                    elvis_result:new(rule, Rule, SortResults);
                true ->
                    elvis_result:new(rule, Rule, [])
            end
        catch
            _:Reason:Stacktrace ->
                Msg = "'~p' while applying rule '~p': ~p",
                elvis_result:new(error, Msg, [
                    Reason, {elvis_rule:ns(Rule), elvis_rule:name(Rule)}, Stacktrace
                ])
        end,
    {[RuleResult | Result], ElvisConfig, File}.

elvis_result_status(Results) ->
    case elvis_result:status(Results) of
        fail ->
            {error, elvis_result:clean(Results)};
        ok ->
            ok
    end.
