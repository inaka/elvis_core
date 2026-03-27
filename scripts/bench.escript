#!/usr/bin/env escript
%%% Benchmark elvis_core linting on a target project.
%%% Usage: ./bench.escript [/path/to/project] [runs]
%%% Defaults: project = this directory, runs = 3

-mode(compile).

main(Args) ->
    {ProjectDir, Runs} = parse_args(Args),
    {ok, OldCwd} = file:get_cwd(),
    ok = setup_code_paths(),
    {ok, _} = application:ensure_all_started(elvis_core),

    ok = file:set_cwd(ProjectDir),

    %% Suppress elvis output
    application:set_env(elvis_core, no_output, true),

    %% Count files
    {ok, ConfigTerms} = file:consult("elvis.config"),
    ElvisConfigs = proplists:get_value(config, hd(ConfigTerms)),
    io:format("Project: ~s~n", [ProjectDir]),
    io:format("Config groups: ~b~n", [length(ElvisConfigs)]),
    io:format("Runs: ~b~n~n", [Runs]),

    %% Warmup
    _ = elvis_core:rock({config_file, "elvis.config"}),

    %% Benchmark total time
    io:format("=== Total lint time ===~n"),
    Times = lists:map(
        fun(_) ->
            {USec, _} = timer:tc(fun() -> elvis_core:rock({config_file, "elvis.config"}) end),
            MSec = USec / 1000,
            io:format("  run: ~.1f ms~n", [MSec]),
            USec
        end,
        lists:seq(1, Runs)
    ),
    AvgMs = lists:sum(Times) / length(Times) / 1000,
    MinMs = lists:min(Times) / 1000,
    io:format("  avg: ~.1f ms, min: ~.1f ms~n~n", [AvgMs, MinMs]),

    %% Per-rule profiling
    io:format("=== Per-rule breakdown (single run) ===~n"),
    RuleTimes = profile_per_rule(),
    print_rule_times(RuleTimes),

    %% eprof
    io:format("~n=== eprof top 20 functions ===~n"),
    profile_eprof(),

    file:set_cwd(OldCwd),
    ok.

profile_per_rule() ->
    {ok, ConfigTerms} = file:consult("elvis.config"),
    ElvisConfigs0 = proplists:get_value(config, hd(ConfigTerms)),
    ok = elvis_config:validate(ElvisConfigs0, "elvis.config"),
    ElvisConfigs = lists:map(fun elvis_config:resolve_files/1, ElvisConfigs0),
    lists:foldl(
        fun(ElvisConfig, Acc) ->
            Files = elvis_config:files(ElvisConfig),
            lists:foldl(
                fun(File, Acc1) ->
                    case elvis_core:do_rock(File, ElvisConfig) of
                        {ok, _} -> profile_file_rules(ElvisConfig, File, Acc1);
                        _ -> Acc1
                    end
                end,
                Acc,
                Files
            )
        end,
        #{},
        ElvisConfigs
    ).

profile_file_rules(ElvisConfig, File, Acc) ->
    LoadedFile = elvis_file:load_file_data(ElvisConfig, File),
    Rules = elvis_config:rules(ElvisConfig),
    {ParseTree, _} = elvis_file:parse_tree(LoadedFile, ElvisConfig),
    {nodes, ElvisAttrs} = elvis_code:find(#{of_types => [elvis], inside => ParseTree}),
    ElvisAttrRules = [Rule || ElvisAttr <- ElvisAttrs, Rule <- ktn_code:attr(value, ElvisAttr)],
    MergedRules = elvis_config:merge_rules(ElvisAttrRules, lists:flatten(Rules)),
    lists:foldl(
        fun(Rule, Acc1) ->
            AnalyzedModule = elvis_file:module(LoadedFile),
            IgnorableModule = elvis_rule:ignorable(AnalyzedModule),
            case elvis_rule:ignored(IgnorableModule, Rule) of
                true ->
                    Acc1;
                false ->
                    RuleWithFile = elvis_rule:file(Rule, LoadedFile),
                    {USec, _} = timer:tc(
                        fun() -> (catch elvis_rule:execute(RuleWithFile, ElvisConfig)) end
                    ),
                    Key = {elvis_rule:ns(Rule), elvis_rule:name(Rule)},
                    {PrevTime, PrevCount} = maps:get(Key, Acc1, {0, 0}),
                    Acc1#{Key => {PrevTime + USec, PrevCount + 1}}
            end
        end,
        Acc,
        MergedRules
    ).

print_rule_times(RuleTimes) ->
    Sorted = lists:reverse(
        lists:keysort(
            2,
            [{Rule, Time, Count} || {Rule, {Time, Count}} <- maps:to_list(RuleTimes)]
        )
    ),
    lists:foreach(
        fun({Rule, Time, Count}) ->
            Ms = Time / 1000,
            case Ms >= 0.1 of
                true -> io:format("  ~8.1f ms  (~b calls)  ~p~n", [Ms, Count, Rule]);
                false -> ok
            end
        end,
        Sorted
    ),
    TotalMs = lists:sum([T || {_, T, _} <- Sorted]) / 1000,
    io:format("  ----------~n"),
    io:format("  ~8.1f ms  total~n", [TotalMs]).

profile_eprof() ->
    eprof:start(),
    eprof:start_profiling([self()]),
    elvis_core:rock({config_file, "elvis.config"}),
    eprof:stop_profiling(),
    eprof:analyze(total, [{sort, time}]),
    eprof:stop().

parse_args([]) ->
    ScriptDir = filename:dirname(filename:absname(escript:script_name())),
    {ScriptDir, 3};
parse_args([Dir]) ->
    {Dir, 3};
parse_args([Dir, RunsStr | _]) ->
    {Dir, list_to_integer(RunsStr)}.

setup_code_paths() ->
    BaseDir = filename:absname(filename:dirname(escript:script_name())),
    LibDir = filename:join([BaseDir, "_build", "default", "lib"]),
    case file:list_dir(LibDir) of
        {ok, Libs} ->
            lists:foreach(
                fun(Lib) ->
                    EbinDir = filename:join([LibDir, Lib, "ebin"]),
                    case filelib:is_dir(EbinDir) of
                        true -> code:add_pathz(EbinDir);
                        false -> ok
                    end
                end,
                Libs
            );
        {error, _} ->
            io:format("Run 'rebar3 compile' first~n"),
            halt(1)
    end,
    ok.
