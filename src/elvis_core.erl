-module(elvis_core).

%% Public API

-export([
         rock/0,
         rock/1,
         rock_this/1,
         rock_this/2
        ]).

-export([start/0]).

-define(APP_NAME, "elvis").

-type source_filename() :: nonempty_string().
-type target() :: source_filename() | module().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Used when starting the application on the shell.
-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(elvis),
    ok.

%%% Rock Command

-spec rock() -> ok | {fail, [elvis_result:file()]}.
rock() ->
    Config = elvis_config:default(),
    rock(Config).

-spec rock(elvis_config:config()) -> ok | {fail, [elvis_result:file()]}.
rock(Config) ->
    elvis_config:validate(Config),
    NewConfig = elvis_config:normalize(Config),
    Results = lists:map(fun do_rock/1, NewConfig),
    lists:foldl(fun combine_results/2, ok, Results).

-spec rock_this(target()) ->
    ok | {fail, elvis_result:file()}.
rock_this(Target) ->
    Config = elvis_config:default(),
    rock_this(Target, Config).

-spec rock_this(target(), elvis_config:config()) ->
    ok | {fail, elvis_result:file()}.
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
    FilteredConfig = lists:filter(FilterFun, NewConfig),

    LoadedFile = load_file_data(FilteredConfig, File),

    ApplyRulesFun = fun(Cfg) -> apply_rules(Cfg, LoadedFile) end,
    Results = lists:map(ApplyRulesFun, FilteredConfig),
    elvis_result_status(Results).

%% @private
-spec do_rock(Config0::elvis_config:config()) ->
    ok | {fail, [elvis_result:file()]}.
do_rock(Config0) ->
    elvis_utils:info("Loading files..."),
    Config = elvis_config:resolve_files(Config0),
    Files = elvis_config:files(Config),
    Fun = fun (File) -> load_file_data(Config, File) end,
    LoadedFiles = lists:map(Fun, Files),
    elvis_utils:info("Applying rules..."),
    Results = [apply_rules(Config, File) || File <- LoadedFiles],
    elvis_result_status(Results).

%% @private
-spec load_file_data(elvis_config:config(), elvis_file:file()) ->
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

-spec apply_rules(Config::elvis_config:config(), File::elvis_file:file()) ->
    elvis_result:file().
apply_rules(Config, File) ->
    Rules = elvis_config:rules(Config),
    Acc = {[], Config, File},
    {RulesResults, _, _} = lists:foldl(fun apply_rule/2, Acc, Rules),

    Results = elvis_result:new(file, File, RulesResults),
    elvis_result:print(Results),
    Results.

apply_rule({Module, Function}, {Result, Config, File}) ->
    apply_rule({Module, Function, #{}}, {Result, Config, File});
apply_rule({Module, Function, ConfigArgs}, {Result, Config, File}) ->
    ConfigMap = ensure_config_map(Module, Function, ConfigArgs),
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
