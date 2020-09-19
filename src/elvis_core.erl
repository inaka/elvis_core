-module(elvis_core).

%% Public API

-export([ rock/1
        , rock_this/2
        ]).

-export([start/0]).

%% for internal use only
-export([do_rock/2]).

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

%%% Rock Command

-spec rock(elvis_config:config()) -> ok | {fail, [elvis_result:file()]}.
rock(Config) ->
    ok = elvis_config:validate(Config),
    NewConfig = elvis_config:normalize(Config),
    Results = lists:map(fun do_parallel_rock/1, NewConfig),
    lists:foldl(fun combine_results/2, ok, Results).

-spec rock_this(target(), elvis_config:config()) ->
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
    File = case elvis_file:find_files([Dirname], Filename) of
               [] -> throw({enoent, Path});
               [File0] -> File0
           end,

    FilterFun = fun(Cfg) ->
                        Filter = elvis_config:filter(Cfg),
                        Dirs = elvis_config:dirs(Cfg),
                        IgnoreList = elvis_config:ignore(Cfg),
                        [] =/= elvis_file:filter_files([File], Dirs, Filter,
                                                       IgnoreList)
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
-spec do_parallel_rock(map()) -> ok | {fail, [elvis_result:file() | elvis_result:rule()]}.
do_parallel_rock(Config0) ->
    Parallel = application:get_env(elvis_core, parallel, 1),
    Config = elvis_config:resolve_files(Config0),
    Files = elvis_config:files(Config),

    {ok, Results} =
        elvis_task:chunk_fold({?MODULE, do_rock},
                              fun(Elem, Acc) ->
                                      elvis_result:print_results(Elem),
                                      {ok, [Elem | Acc]}
                              end,
                              [], [Config], Files, Parallel),
    elvis_result_status(Results).

-spec do_rock(elvis_file:file(), map() | [map()]) -> {ok, elvis_result:file()}.
do_rock(File, Config) ->
    LoadedFile = load_file_data(Config, File),
    Results = apply_rules(Config, LoadedFile),
    {ok, Results}.

%% @private
-spec load_file_data(map() | [map()], elvis_file:file()) -> elvis_file:file().
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

-spec apply_rules(map(), File::elvis_file:file()) ->
    elvis_result:file().
apply_rules(Config, File) ->
    Rules = elvis_config:rules(Config),
    Acc = {[], Config, File},
    {ParseTree, _} = elvis_file:parse_tree(Config, File),
    {RulesResults, _, _} = lists:foldl(fun apply_rule/2, Acc,
                                       merge_rules(Rules, {file, ParseTree})),
    elvis_result:new(file, File, RulesResults).

merge_rules(ElvisConfigRules, {file, ParseTree}) ->
    ElvisAttrs = elvis_code:find(fun is_elvis_attr/1, ParseTree,
                                 #{ traverse => content, mode => node }),
    ElvisAttrRules = elvis_attr_rules(ElvisAttrs),
    merge_rules(ElvisAttrRules, ElvisConfigRules);
merge_rules(ElvisAttrRules, undefined = _ElvisConfigRules) ->
    ElvisAttrRules;
merge_rules(undefined = _ElvisAttrRules, ElvisConfigRules) ->
    ElvisConfigRules;
merge_rules(ElvisAttrRules, ElvisConfigRules) ->
    lists:filter(
        fun (ElvisConfigRule) ->
            not(lists:any(
                    fun (ElvisAttrRule) ->
                        {erlang:element(1, ElvisAttrRule), erlang:element(2, ElvisAttrRule)}
                            =:=
                        {erlang:element(1, ElvisConfigRule), erlang:element(2, ElvisConfigRule)}
                    end,
                    ElvisAttrRules))
        end,
        ElvisConfigRules) ++ ElvisAttrRules.

is_elvis_attr(Node) ->
    ktn_code:type(Node) =:= elvis.

elvis_attr_rules([] = _ElvisAttrs) ->
    undefined;
elvis_attr_rules(ElvisAttrs) ->
    lists:foldl(
        fun (ElvisAttr, ModuleLevelRules) ->
            ModuleLevelRules ++ try_attr_rules(ktn_code:attr(value, ElvisAttr))
        end,
        [],
        ElvisAttrs).

try_attr_rules(ModuleLevelRules) when is_map(ModuleLevelRules) ->
    maps:get(module, ModuleLevelRules, []);
try_attr_rules(_ModuleLevelRules) ->
    [].

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
    RuleResult = try
                    Results = Module:Function(Config, File, ConfigMap),
                    SortFun = fun(#{line_num := L1}, #{line_num := L2}) ->
                                  L1 =< L2
                              end,
                    SortResults = lists:sort(SortFun, Results),
                    elvis_result:new(rule, Function, SortResults)
                 catch
                     _:Reason ->
                         Msg = "'~p' while applying rule '~p'.",
                         elvis_result:new(error, Msg, [Reason, Function])
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
ensure_config_map(elvis_style,
                  module_naming_convention,
                  [Regex, IgnoreModules]) ->
    #{regex => Regex, ignore => IgnoreModules};
ensure_config_map(_, _, []) ->
    #{}.

%% @private
elvis_result_status(Results) ->
    case elvis_result:status(Results) of
        fail -> {fail, elvis_result:clean(Results)};
        ok -> ok
    end.
