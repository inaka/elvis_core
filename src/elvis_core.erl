-module(elvis_core).

%% Public API

-export([rock/1, rock_this/2]).
-export([start/0]).
%% for internal use only
-export([do_rock/2]).
%% for eating our own dogfood
-export([main/1]).

-export_type([target/0]).

-ifdef(TEST).

-export([apply_rule/2]).

-endif.

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
-spec rock([elvis_config:t()]) ->
    ok | {fail, [{throw, term()} | elvis_result:file() | elvis_result:rule()]}.
rock(ElvisConfig) ->
    ok = elvis_config:validate_config(ElvisConfig),
    elvis_ruleset:dump_custom(),
    Results = lists:map(fun do_parallel_rock/1, ElvisConfig),
    lists:foldl(fun combine_results/2, ok, Results).

-spec rock_this(target(), [elvis_config:t()]) ->
    ok | {fail, [elvis_result:file() | elvis_result:rule()]}.
rock_this(Module, ElvisConfig) when is_atom(Module) ->
    ModuleInfo = Module:module_info(compile),
    Path = proplists:get_value(source, ModuleInfo),
    rock_this(Path, ElvisConfig);
rock_this(Path, ElvisConfig) ->
    ok = elvis_config:validate_config(ElvisConfig),
    elvis_ruleset:dump_custom(),
    Dirname = filename:dirname(Path),
    Filename = filename:basename(Path),
    File =
        case elvis_file:find_files([Dirname], Filename) of
            [] ->
                throw({enoent, Path});
            [File0] ->
                File0
        end,

    FilterFun =
        fun(Cfg) ->
            Filter = elvis_config:filter(Cfg),
            Dirs = elvis_config:dirs(Cfg),
            IgnoreList = elvis_config:ignore(Cfg),
            [] =/= elvis_file:filter_files([File], Dirs, Filter, IgnoreList)
        end,
    case lists:filter(FilterFun, ElvisConfig) of
        [] ->
            elvis_utils:output(info, "Skipping ~s", [Path]);
        FilteredElvisConfig ->
            LoadedFile = load_file_data(FilteredElvisConfig, File),
            ApplyRulesFun = fun(Cfg) -> apply_rules_and_print(Cfg, LoadedFile) end,
            Results = lists:map(ApplyRulesFun, FilteredElvisConfig),
            elvis_result_status(Results)
    end.

%% In this context, `throw` means an error, e.g., validation or internal, not an actual
%% call to `erlang:throw/1`.
-spec do_parallel_rock(elvis_config:t()) ->
    ok
    | {fail, [{throw, term()} | elvis_result:file() | elvis_result:rule()]}.
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
        {error, {T, E}} ->
            %% {T, E} will be put into an {error, _} tuple higher on the call stack,
            %% let's not encapsulate it multiple times.
            {fail, [{T, E}]}
    end.

-spec do_rock(elvis_file:t(), [elvis_config:t()] | elvis_config:t()) ->
    {ok, elvis_result:file()}.
do_rock(File, ElvisConfig) ->
    LoadedFile = load_file_data(ElvisConfig, File),
    Results = apply_rules(ElvisConfig, LoadedFile),
    {ok, Results}.

-spec load_file_data([elvis_config:t()] | elvis_config:t(), elvis_file:t()) ->
    elvis_file:t().
load_file_data(ElvisConfig, File) ->
    Path = elvis_file:path(File),
    elvis_utils:output(info, "Loading ~s", [Path]),
    try
        elvis_file:load_file_data(ElvisConfig, File)
    catch
        _:Reason ->
            Msg = "~p when loading file ~p.",
            elvis_utils:output(error, Msg, [Reason, Path]),
            File
    end.

-spec main([]) -> true | no_return().
main([]) ->
    ok = application:load(elvis_core),
    {module, _} = code:ensure_loaded(elvis_style),
    case rock(elvis_config:config()) of
        ok -> true;
        _ -> halt(1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec combine_results(
    ok | {fail, [elvis_result:file()]},
    ok | {fail, [elvis_result:file()]}
) ->
    ok | {fail, [elvis_result:file()]}.
combine_results(ok, Acc) ->
    Acc;
combine_results(Item, ok) ->
    Item;
combine_results({fail, ItemResults}, {fail, AccResults}) ->
    {fail, ItemResults ++ AccResults}.

apply_rules_and_print(ElvisConfig, File) ->
    Results = apply_rules(ElvisConfig, File),
    elvis_result:print_results(Results),
    Results.

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
            {fail, elvis_result:clean(Results)};
        ok ->
            ok
    end.
