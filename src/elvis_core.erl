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
    ok = elvis_config:validate(Config),
    NewConfig = elvis_config:normalize(Config),
    Results = lists:map(fun do_parallel_rock/1, NewConfig),
    lists:foldl(fun combine_results/2, ok, Results).

-spec rock_this(target()) ->
    ok | {fail, [elvis_result:file() | elvis_result:rule()]}.
rock_this(Target) ->
    Config = elvis_config:default(),
    rock_this(Target, Config).

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
    Parallel = application:get_env(elvis, parallel, 1),
    Config = elvis_config:resolve_files(Config0),
    Files = elvis_config:files(Config),

    Results = do_parallel_rock0(Config, Files, Parallel),
    elvis_result_status(Results).

do_parallel_rock0(Config, Files, N) ->
    do_parallel_rock1(Config, Files, N, N, [], []).

do_parallel_rock1(_Config, [], _MaxW, _RemainW, AccR, AccG) ->
    gather_all_results(AccR, AccG);
do_parallel_rock1(Config, FilesList, MaxW, 0, AccR, AccG) ->
    {AccR1, AccG1, N} = gather_results(AccR, AccG),
    do_parallel_rock1(Config, FilesList, MaxW, erlang:min(N, MaxW), AccR1, AccG1);
do_parallel_rock1(Config, FilesList, MaxW, RemainW, AccR, AccG) ->
    {WorkToBeDone, FilesRemain} =
        try lists:split(RemainW, FilesList) of
            Res -> Res
        catch error:badarg -> {FilesList, []}
        end,

    Gather = [do_rock_worker(Config, File) || File <- WorkToBeDone],
    do_parallel_rock1(Config, FilesRemain, MaxW, 0, AccR, Gather ++ AccG).

do_rock_worker(Config, #{path := Path} = File) ->
    Parent = self(),
    Key = spawn_monitor(fun() -> do_rock(Parent, Config, File) end),
    {Key, Path}.

-spec do_rock(pid(), elvis_config:config(), elvis_result:file()) -> no_return().
do_rock(Parent, Config, File) ->
    try
        LoadedFile = load_file_data(Config, File),
        apply_rules(Config, LoadedFile)
    of
        Results ->
            exit({Parent, {ok, Results}})
    catch T:E ->
            exit({Parent, {error, {T,E}}})
    end.

gather_all_results(AccR, Remain) ->
    {AccR1, _, _} = gather_results0(AccR, Remain, 0, infinity),
    AccR1.

gather_results(AccR, AccG) ->
    {Key, Res0} = gather(infinity),
    gather_results0([Res0 | AccR], lists:keydelete(Key, 1, AccG), 1, 0).

gather_results0(AccR, [], N, _Timeout) ->
    {AccR, [], N};
gather_results0(AccR, AccG, N, Timeout) ->
    case gather(Timeout) of
        timeout -> {AccR, AccG, N};
        {Key, Res0} ->
            gather_results0([Res0 | AccR], lists:keydelete(Key, 1, AccG), N + 1, Timeout)
    end.

gather(Timeout) ->
    Self = self(),
    receive
        {'DOWN', MonRef, process, Pid, {Self, Res}} ->
            case Res of
                {ok, Res0} ->
                    elvis_result:print_results(Res0),
                    {{Pid, MonRef}, Res0};
                {error, {T,E}} ->
                    erlang:T(E)
            end
    after Timeout ->
            timeout
    end.

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
    {RulesResults, _, _} = lists:foldl(fun apply_rule/2, Acc, Rules),
    elvis_result:new(file, File, RulesResults).

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
