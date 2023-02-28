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
-type rule_config() :: #{atom() => term()}.
-type rule() ::
    {Module :: module(), Function :: atom(), RuleConfig :: rule_config()} |
    {Module :: module(), Function :: atom()}.

-export_type([rule_config/0, rule/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(elvis_core),
    ok.

%%% Rock Command

-spec rock(elvis_config:configs()) -> ok | {fail, [elvis_result:file()]}.
rock(Config) ->
    ok = elvis_config:validate(Config),
    NewConfig = elvis_config:normalize(Config),
    Results = lists:map(fun do_parallel_rock/1, NewConfig),
    lists:foldl(fun combine_results/2, ok, Results).

-spec rock_this(target(), elvis_config:configs()) ->
                   ok | {fail, [elvis_result:file() | elvis_result:rule()]}.
rock_this(Module, Config) when is_atom(Module) ->
    ModuleInfo = Module:module_info(compile),
    Path = proplists:get_value(source, ModuleInfo),
    rock_this(Path, Config);
rock_this(Path, Config) ->
    elvis_config:validate(Config),
    NewConfig = elvis_config:normalize(Config),
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
    case lists:filter(FilterFun, NewConfig) of
        [] ->
            elvis_utils:info("Skipping ~s", [Path]);
        FilteredConfig ->
            LoadedFile = load_file_data(FilteredConfig, File),
            ApplyRulesFun = fun(Cfg) -> apply_rules_and_print(Cfg, LoadedFile) end,
            Results = lists:map(ApplyRulesFun, FilteredConfig),
            elvis_result_status(Results)
    end.

%% @private
-spec do_parallel_rock(elvis_config:config()) ->
                          ok | {fail, [elvis_result:file() | elvis_result:rule()]}.
do_parallel_rock(Config0) ->
    Parallel = elvis_config:from_application_or_config(parallel, 1),
    Config = elvis_config:resolve_files(Config0),
    Files = elvis_config:files(Config),

    Result =
        elvis_task:chunk_fold({?MODULE, do_rock},
                              fun(Elem, Acc) ->
                                 elvis_result:print_results(Elem),
                                 {ok, [Elem | Acc]}
                              end,
                              [],
                              [Config],
                              Files,
                              Parallel),
    case Result of
        {ok, Results} ->
            elvis_result_status(Results);
        {error, {T, E}} ->
            %% {T, E} will be put into an {error, _} tuple higher on the call stack,
            %% let's not encapsulate it multiple times.
            {fail, [{T, E}]}
    end.

-spec do_rock(elvis_file:file(), elvis_config:configs() | elvis_config:config()) ->
                 {ok, elvis_result:file()}.
do_rock(File, Config) ->
    LoadedFile = load_file_data(Config, File),
    Results = apply_rules(Config, LoadedFile),
    {ok, Results}.

%% @private
-spec load_file_data(elvis_config:configs() | elvis_config:config(), elvis_file:file()) ->
                        elvis_file:file().
load_file_data(Config, File) ->
    Path = elvis_file:path(File),
    elvis_utils:info("Loading ~s", [Path]),
    try
        elvis_file:load_file_data(Config, File)
    catch
        _:Reason ->
            Msg = "~p when loading file ~p.",
            elvis_utils:error_prn(Msg, [Reason, Path]),
            File
    end.

%% @private
-spec main([]) -> true | no_return().
main([]) ->
    ok = application:load(elvis_core),
    R = rock(elvis_config:from_file("elvis.config")),
    R =:= ok orelse halt(1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec combine_results(ok | {fail, [elvis_result:file()]},
                      ok | {fail, [elvis_result:file()]}) ->
                         ok | {fail, [elvis_result:file()]}.
combine_results(ok, Acc) ->
    Acc;
combine_results(Item, ok) ->
    Item;
combine_results({fail, ItemResults}, {fail, AccResults}) ->
    {fail, ItemResults ++ AccResults}.

apply_rules_and_print(Config, File) ->
    Results = apply_rules(Config, File),
    elvis_result:print_results(Results),
    Results.

-spec apply_rules(elvis_config:configs() | elvis_config:config(),
                  File :: elvis_file:file()) ->
                     elvis_result:file().
apply_rules(Config, File) ->
    Rules = elvis_config:rules(Config),
    Acc = {[], Config, File},
    {ParseTree, _} = elvis_file:parse_tree(Config, File),
    {RulesResults, _, _} =
        lists:foldl(fun apply_rule/2, Acc, merge_rules({file, ParseTree}, lists:flatten(Rules))),
    elvis_result:new(file, File, RulesResults).

merge_rules({file, ParseTree}, ElvisConfigRules) ->
    ElvisAttrs =
        elvis_code:find(fun is_elvis_attr/1, ParseTree, #{traverse => content, mode => node}),
    ElvisAttrRules = elvis_attr_rules(ElvisAttrs),
    elvis_config:merge_rules(ElvisAttrRules, ElvisConfigRules).

is_elvis_attr(Node) ->
    ktn_code:type(Node) =:= elvis.

elvis_attr_rules([] = _ElvisAttrs) ->
    [];
elvis_attr_rules(ElvisAttrs) ->
    [Rule || ElvisAttr <- ElvisAttrs, Rule <- ktn_code:attr(value, ElvisAttr)].

-spec apply_rule({Mod, Fun} | {Mod, Fun, RuleCfg}, {Results, ElvisCfg, File}) -> Result
    when Mod :: module(),
         Fun :: atom(),
         RuleCfg :: rule_config(),
         Results :: [elvis_result:rule()],
         ElvisCfg :: elvis_config:config(),
         File :: elvis_file:file(),
         Result :: {Results, ElvisCfg, File}.
apply_rule({Module, Function}, {Result, Config, File}) ->
    apply_rule({Module, Function, #{}}, {Result, Config, File});
apply_rule({Module, Function, ConfigArgs}, {Result, Config, File}) ->
    ConfigMap =
        try
            ensure_config_map(Module, Function, ConfigArgs)
        catch
            _:function_clause ->
                throw({invalid_config, disable_without_ruleset})
        end,
    RuleResult =
        try
            AnalyzedModule = elvis_file:module(File),
            Ignores = maps:get(ignore, ConfigMap, []),
            case lists:member(AnalyzedModule, Ignores) of
                false ->
                    FilteredConfigMap =
                        maps:merge(ConfigMap#{ignore => lists:delete(AnalyzedModule, Ignores)},
                                   ConfigArgs),
                    Results = Module:Function(Config, File, FilteredConfigMap),
                    SortFun = fun(#{line_num := L1}, #{line_num := L2}) -> L1 =< L2 end,
                    SortResults = lists:sort(SortFun, Results),
                    elvis_result:new(rule, {Module, Function}, SortResults);
                true ->
                    elvis_result:new(rule, {Module, Function}, [])
            end
        catch
            _:Reason:Stacktrace ->
                Msg = "'~p' while applying rule '~p': ~p.",
                elvis_result:new(error, Msg, [Reason, Function, Stacktrace])
        end,
    {[RuleResult | Result], Config, File}.

%% @doc Process a tules configuration argument and converts it to a map.
ensure_config_map(_, _, Map) when is_map(Map) ->
    Map;
ensure_config_map(elvis_style, line_length, [Limit]) ->
    #{limit => Limit};
ensure_config_map(elvis_style, operator_spaces, Rules) ->
    #{rules => Rules};
ensure_config_map(elvis_style, nesting_level, [Level]) ->
    #{level => Level};
ensure_config_map(elvis_style, god_modules, [Limit]) ->
    #{limit => Limit};
ensure_config_map(elvis_style, god_modules, [Limit, IgnoreModules]) ->
    #{limit => Limit, ignore => IgnoreModules};
ensure_config_map(elvis_style, invalid_dynamic_call, IgnoreModules) ->
    #{ignore => IgnoreModules};
ensure_config_map(elvis_style, module_naming_convention, [Regex, IgnoreModules]) ->
    #{regex => Regex, ignore => IgnoreModules};
ensure_config_map(_, _, []) ->
    #{}.

%% @private
elvis_result_status(Results) ->
    case elvis_result:status(Results) of
        fail ->
            {fail, elvis_result:clean(Results)};
        ok ->
            ok
    end.
