-module(elvis_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, chunk_fold_task/2]).
-export([
    rock_with_empty_map_config/1,
    rock_with_empty_list_config/1,
    rock_with_incomplete_config/1,
    rock_with_list_config/1,
    rock_without_colors/1,
    rock_with_parsable/1,
    rock_with_no_output_has_no_output/1,
    rock_with_non_parsable_file/1,
    rock_with_errors_has_output/1,
    rock_without_errors_has_no_output/1,
    rock_without_errors_and_with_verbose_has_output/1,
    rock_with_rule_groups/1,
    rock_with_glob_dirs_not_matching/1,
    rock_with_umbrella_apps/1,
    custom_ruleset/1,
    hrl_ruleset/1,
    find_file_and_check_src/1,
    find_file_with_ignore/1,
    ignore_gitignored/1,
    invalid_file/1,
    to_string/1,
    chunk_fold/1,
    rock_with_invalid_rules/1,
    rock_with_removed_rules/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    Exports = ?MODULE:module_info(exports),
    [
        F
     || {F, _} <- Exports,
        not lists:member(F, [chunk_fold_task | elvis_test_utils:excluded_funs_all()])
    ].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(elvis_core),
    Config.

end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Rocking

rock_with_empty_map_config(_Config) ->
    {errors, "in 'config', at list position number 1, 'files' is a compulsory option."} = elvis_core:rock(
        [#{}]
    ),
    {errors, "'config' is expected to exist and be a non-empty list."} = elvis_core:rock([]),
    ok.

rock_with_empty_list_config(_Config) ->
    {errors, "in 'config', at list position number 1, 'files' is a compulsory option."} = elvis_core:rock(
        [#{}, #{}]
    ),
    ok.

rock_with_incomplete_config(_Config) ->
    ElvisConfig = [#{files => ["src/*.erl"]}],
    {errors,
        "in 'config', at list position number 1, no '<globs>' in 'src/*.erl' yielded any files to analyse."} = elvis_core:rock(
        ElvisConfig
    ),
    ok.

rock_with_list_config(_Config) ->
    ElvisConfig = [
        #{
            files => ["../../../../test/dirs/src/*.erl"],
            rules => [{elvis_text_style, line_length, disable}]
        }
    ],
    ok = elvis_core:rock(ElvisConfig).

%% Regression test for https://github.com/inaka/elvis_core/issues/543
%% Glob patterns that don't match any existing paths should not cause
%% a validation error as long as at least one glob yields files.
rock_with_glob_dirs_not_matching(_Config) ->
    ElvisConfig = [
        #{
            files =>
                [
                    "absolutely/very/nonexistent/**/src/*.erl",
                    "../../../../test/dirs/src/*.erl"
                ],
            rules => [{elvis_text_style, line_length, disable}]
        }
    ],
    ok = elvis_core:rock(ElvisConfig).

rock_without_colors(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Expected = "\\e.*?m",
    ok =
        try
            elvis_test_utils:check_some_line_output(
                Fun, Expected, fun elvis_test_utils:matches_regex/2
            )
        of
            Result ->
                ct:fail("Unexpected result ~p", [Result])
        catch
            _:{badmatch, []} ->
                ok
        end.

rock_with_parsable(_Config) ->
    {ok, Default} = application:get_env(elvis_core, output_format),
    application:set_env(elvis_core, output_format, parsable),
    ElvisConfig = elvis_test_utils:config(),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Expected = ".*\\.erl:\\d:[a-zA-Z0-9_]+:.*",
    ok =
        try
            elvis_test_utils:check_some_line_output(
                Fun, Expected, fun elvis_test_utils:matches_regex/2
            )
        of
            Result ->
                io:format("~p~n", [Result])
        catch
            _:{badmatch, []} ->
                ct:fail("Unexpected result ~p")
        after
            application:set_env(elvis_core, output_format, Default)
        end.

rock_with_non_parsable_file(_Config) ->
    Path =
        "../../../../_build/test/lib/elvis_core/test/non_compilable_examples/fail_non_parsable_file.erl",
    ElvisConfig = elvis_test_utils:config_erl_files(Path),
    {errors, [{error, Result}]} = elvis_core:rock(ElvisConfig),
    "{{red}}Error: {{reset}}{badmatch," ++ _ = Result,

    ok.

rock_with_no_output_has_no_output(_Config) ->
    application:set_env(elvis_core, no_output, true),
    ElvisConfig = elvis_test_utils:config(),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    [] = get_output(Fun),
    application:unset_env(elvis_core, no_output),
    ok.

rock_with_errors_has_output(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Expected = "FAIL",
    [_ | _] = elvis_test_utils:check_some_line_output(
        Fun, Expected, fun elvis_test_utils:matches_regex/2
    ),
    ok.

rock_without_errors_has_no_output(_Config) ->
    ConfigPath = "../../../../config/test.pass.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Output = get_output(Fun),
    %% This is related to the test case `rock_with_non_parsable_file`,
    %% which will print an error to the standard output
    %% and CT will capture it.
    %% Thus, we remove it from the list of captures before doing the actual check
    RemoveSearchPattern = "fail_non_parsable_file.erl",
    ct:log("Output=~p~n", [Output]),
    [] =
        lists:filter(
            fun(String) -> string:find(String, RemoveSearchPattern) == nomatch end,
            Output
        ),
    ok.

rock_without_errors_and_with_verbose_has_output(_Config) ->
    application:set_env(elvis_core, verbose, true),
    ElvisConfig = elvis_test_utils:config(),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Expected = "OK",
    [_ | _] = elvis_test_utils:check_some_line_output(
        Fun, Expected, fun elvis_test_utils:matches_regex/2
    ),
    application:unset_env(elvis_core, verbose),
    ok.

rock_with_rule_groups(_Config) ->
    % elvis_config will load default elvis_core rules for every
    % rule_group in the configuration
    RulesGroupConfig =
        [
            #{
                files => ["../../../../test/dirs/apps/app3/src/*.erl"],
                ruleset => erl_files
            },
            #{
                files => ["../../../../test/dirs/apps/app3/include/*.hrl"],
                ruleset => hrl_files
            },
            #{
                files => ["../../../../test/dirs/apps/app3/rebar.config"],
                ruleset => rebar_config
            }
        ],
    ok = elvis_core:rock(RulesGroupConfig),
    % Override default elvis_core rules without ruleset should fail.
    OverrideFailConfig =
        [
            #{
                files => ["src/*.erl"],
                rules =>
                    [
                        {elvis_text_style, line_length, #{limit => 90}},
                        {elvis_style, state_record_and_type, disable}
                    ]
            }
        ],
    {errors,
        "in 'config', at list position number 1, no '<globs>' in 'src/*.erl' yielded any files to analyse."} = elvis_core:rock(
        OverrideFailConfig
    ),
    % Override default elvis_core rules.
    OverrideConfig =
        [
            #{
                files => ["../../../../test/dirs/apps/app3/src/*.erl"],
                ruleset => erl_files,
                rules =>
                    % I like 90 chars per line.
                    [
                        {elvis_text_style, line_length, #{limit => 90}},
                        % I like tabs so disable this rule.
                        {elvis_text_style, no_tabs, disable}
                    ]
            },
            #{
                files => ["../../../../test/dirs/apps/app3/rebar.config"],
                ruleset => rebar_config
            }
        ],
    ok = elvis_core:rock(OverrideConfig).

rock_with_umbrella_apps(_Config) ->
    ElvisUmbrellaConfigFile = "../../../../config/elvis-umbrella.config",
    ElvisConfig = elvis_config:from_file(ElvisUmbrellaConfigFile),
    {errors, Failures} = elvis_core:rock(ElvisConfig),
    [
        #{
            file :=
                "../../../../_build/test/lib/elvis_core/test/dirs/apps/app1/src/dirs_apps_app1_src.erl"
        },
        #{
            file :=
                "../../../../_build/test/lib/elvis_core/test/dirs/apps/app1/test/dirs_apps_app1_test.erl"
        },
        #{
            file :=
                "../../../../_build/test/lib/elvis_core/test/dirs/apps/app2/src/dirs_apps_app2_src.erl"
        },
        #{
            file :=
                "../../../../_build/test/lib/elvis_core/test/dirs/apps/app2/test/dirs_apps_app2_test.erl"
        },
        #{
            file :=
                "../../../../_build/test/lib/elvis_core/test/dirs/apps/app3/src/app3_example.erl"
        },
        #{file := "../../../../_build/test/lib/elvis_core/test/dirs/src/dirs_src.erl"},
        #{file := "../../../../_build/test/lib/elvis_core/test/dirs/test/dir_test.erl"}
    ] = lists:sort(Failures),
    ok.

rock_with_invalid_rules(_Config) ->
    ConfigPath = "../../../../test/examples/invalid_rules.elvis.config",
    {error, Error} = elvis_config:from_file(ConfigPath),
    Error =
        "in 'config', at list position number 1, no '<globs>' in '../../_build/default/lib/elvis_core/src/*.erl' yielded any files to analyse.",
    ok.

rock_with_removed_rules(_Config) ->
    ConfigPath = "../../../../test/examples/removed_rules.elvis.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    %% Should not crash; removed rules are skipped with a warning
    ok = elvis_core:rock(ElvisConfig).

%%%%%%%%%%%%%%%
%%% Utils

custom_ruleset(_Config) ->
    ConfigPath = "../../../../config/elvis-test-custom-ruleset.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    NoTabs = elvis_rule:new(elvis_text_style, no_tabs),
    [[NoTabs]] = elvis_config:rules(ElvisConfig),

    %% this is also done by elvis_core:rock/1
    _ = elvis_ruleset:drop_custom(),

    %% read unknown ruleset configuration to ensure rulesets from
    %% previous load do not stick around
    ConfigPathMissing = "../../../../config/elvis-test-unknown-ruleset.config",
    {error,
        "in 'config', at list position number 1, 'project' is expected to be either a custom or a default ruleset."} = elvis_config:from_file(
        ConfigPathMissing
    ),
    ok.

hrl_ruleset(_Config) ->
    ConfigPath = "../../../../config/elvis-test-hrl-files.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    {errors, [
        #{
            file := "../../../../_build/test/lib/elvis_core/test/examples/test_good.hrl",
            rules := []
        },
        #{
            file := "../../../../_build/test/lib/elvis_core/test/examples/test_bad.hrl",
            rules := [#{name := line_length}]
        }
    ]} =
        elvis_core:rock(ElvisConfig),
    ok.

find_file_and_check_src(_Config) ->
    Globs = ["../../../../test/examples/*.erl"],

    [] = elvis_file:find_files(["../../../../test/examples/doesnt_exist.erl"]),
    Files = elvis_file:find_files(Globs),
    [File] = [F || F <- Files, filename:basename(elvis_file:path(F)) =:= "small.erl"],

    {<<"-module(small).", LineBreak/binary>>, _} = elvis_file:src(File),
    LineBreak =
        case os:type() of
            {unix, _} ->
                <<"\n">>;
            {win32, _} ->
                <<"\r\n">>
        end,
    {error, enoent} = elvis_file:src(#{path => "doesnt_exist.erl"}).

find_file_with_ignore(_Config) ->
    Globs = ["../../../../test/examples/find_test*.erl"],
    Ignore = elvis_config:ignore(#{ignore => [find_test1]}),
    Files = [_, _] = elvis_file:find_files(Globs),
    [#{path := "../../../../test/examples/find_test2.erl"}] =
        elvis_file:filter_files(Files, Globs, Ignore).

ignore_gitignored(_Config) ->
    ConfigPath = "../../../../config/elvis-test-hrl-files.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    WithGitignore = elvis_config:inject_ignore(ElvisConfig, ["test_good.hrl", "test_bad.hrl"]),
    ok = elvis_core:rock(WithGitignore).

invalid_file(_Config) ->
    ok =
        try
            {error, _} = elvis_file:src(#{}),
            fail
        catch
            {invalid_file, #{}} ->
                ok
        end.

to_string(_Config) ->
    "1" = elvis_utils:to_str(1),
    "hello" = elvis_utils:to_str(<<"hello">>),
    "atom" = elvis_utils:to_str(atom).

chunk_fold(_Config) ->
    Multiplier = 10,
    List = lists:seq(1, 10),
    {ok, Value} =
        elvis_task:chunk_fold(
            {?MODULE, chunk_fold_task},
            fun(Elem, Acc) -> {ok, Acc + Elem} end,
            0,
            [Multiplier],
            lists:seq(1, 10),
            10
        ),
    Value =
        lists:sum(
            lists:map(fun(E) -> E * Multiplier end, List)
        ),

    {error, {error, undef}} =
        elvis_task:chunk_fold(
            {?MODULE, chunk_fold_task_do_not_exist},
            fun(Elem, Acc) -> {ok, Acc + Elem} end,
            0,
            [Multiplier],
            lists:seq(1, 10),
            10
        ).

chunk_fold_task(Elem, Multiplier) ->
    {ok, Elem * Multiplier}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_output(Fun) ->
    _ = ct:capture_start(),
    _ = Fun(),
    _ = ct:capture_stop(),
    ct:capture_get([]).
