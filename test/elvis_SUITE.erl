-module(elvis_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, chunk_fold_task/2]).
-export([
    rock_with_empty_map_config/1,
    rock_with_empty_list_config/1,
    rock_with_incomplete_config/1,
    rock_with_list_config/1,
    rock_with_file_config/1,
    rock_with_old_config/1,
    rock_with_rebar_default_config/1,
    rock_this/1,
    rock_without_colors/1,
    rock_with_parsable/1,
    rock_with_no_output_has_no_output/1,
    rock_with_non_parsable_file/1,
    rock_with_errors_has_output/1,
    rock_without_errors_has_no_output/1,
    rock_without_errors_and_with_verbose_has_output/1,
    rock_with_rule_groups/1,
    rock_this_skipping_files/1,
    rock_this_not_skipping_files/1,
    rock_with_umbrella_apps/1,
    custom_ruleset/1,
    hrl_ruleset/1,
    throw_configuration/1,
    find_file_and_check_src/1,
    find_file_with_ignore/1,
    invalid_file/1,
    to_string/1,
    chunk_fold/1,
    rock_with_invalid_rules/1
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
    ok =
        try
            ok = elvis_core:rock([#{}]),
            fail
        catch
            {invalid_config, _} ->
                ok
        end,
    ok =
        try
            ok = elvis_core:rock([#{} || X <- lists:seq(1, 10), X < 1]),
            fail
        catch
            {invalid_config, _} ->
                ok
        end.

rock_with_empty_list_config(_Config) ->
    ok =
        try
            ok = elvis_core:rock([#{}, #{}]),
            fail
        catch
            {invalid_config, _} ->
                ok
        end.

rock_with_incomplete_config(_Config) ->
    ElvisConfig = [#{src_dirs => ["src"]}],
    ok =
        try
            ok = elvis_core:rock(ElvisConfig),
            fail
        catch
            {invalid_config, _} ->
                ok
        end.

rock_with_list_config(_Config) ->
    ElvisConfig = [#{src_dirs => ["src"], rules => []}],
    ok =
        try
            ok = elvis_core:rock(ElvisConfig)
        catch
            {invalid_config, _} ->
                fail
        end.

rock_with_file_config(_Config) ->
    ConfigPath = "../../config/elvis.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Expected =
        "# \\.\\./\\.\\./_build/test/lib/elvis_core/test/" ++ "examples/.*\\.erl.*FAIL",
    [_ | _] = check_some_line_output(Fun, Expected, fun matches_regex/2),
    ok.

rock_with_old_config(_Config) ->
    ConfigPath = "../../config/old/elvis.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    ok =
        try
            ok = elvis_core:rock(ElvisConfig)
        catch
            {invalid_config, _} ->
                fail
        end,

    ConfigPath1 = "../../config/old/elvis-test.config",
    ElvisConfig1 = elvis_config:from_file(ConfigPath1),
    ok =
        try
            ok = elvis_core:rock(ElvisConfig1)
        catch
            {invalid_config, _} ->
                fail
        end,

    ConfigPath2 = "../../config/old/elvis-test-rule-config-list.config",
    ElvisConfig2 = elvis_config:from_file(ConfigPath2),
    ok =
        try
            ok = elvis_core:rock(ElvisConfig2)
        catch
            {invalid_config, _} ->
                fail
        end.

rock_with_rebar_default_config(_Config) ->
    {ok, _} = file:copy("../../config/rebar.config", "rebar.config"),
    ElvisConfig = elvis_config:from_rebar("rebar.config"),
    [#{name := line_length}] =
        try
            {fail, Results} = elvis_core:rock(ElvisConfig),
            [Rule || #{rules := [Rule]} <- Results]
        after
            file:delete("rebar.config")
        end,
    ok.

rock_this(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    ok = elvis_core:rock_this(elvis_core, ElvisConfig),

    ok =
        try
            {fail, _} = elvis_core:rock_this("bla.erl", ElvisConfig)
        catch
            _:{enoent, "bla.erl"} ->
                ok
        end,

    Path = "../../_build/test/lib/elvis_core/test/examples/fail_line_length.erl",
    {fail, _} = elvis_core:rock_this(Path, ElvisConfig),

    ok.

rock_without_colors(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Expected = "\\e.*?m",
    ok =
        try check_some_line_output(Fun, Expected, fun matches_regex/2) of
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
        try check_some_line_output(Fun, Expected, fun matches_regex/2) of
            Result ->
                io:format("~p~n", [Result])
        catch
            _:{badmatch, []} ->
                ct:fail("Unexpected result ~p")
        after
            application:set_env(elvis_core, output_format, Default)
        end.

rock_with_non_parsable_file(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    Path =
        "../../_build/test/lib/elvis_core/test/non_compilable_examples/fail_non_parsable_file.erl",
    try
        elvis_core:rock_this(Path, ElvisConfig)
    catch
        {fail, {error, {badmatch, _}}} ->
            ok
    end.

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
    [_ | _] = check_some_line_output(Fun, Expected, fun matches_regex/2),
    ok.

rock_without_errors_has_no_output(_Config) ->
    ConfigPath = "../../config/test.pass.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Output = get_output(Fun),
    %% This is related to the test case `rock_with_non_parsable_file`,
    %% which will print an error to the standard output
    %% and CT will capture it.
    %% Thus, we remove it from the list of captures before doing the actual check
    RemoveSearchPattern = "fail_non_parsable_file.erl",
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
    [_ | _] = check_some_line_output(Fun, Expected, fun matches_regex/2),
    application:unset_env(elvis_core, verbose),
    ok.

rock_with_rule_groups(_Config) ->
    % elvis_config will load default elvis_core rules for every
    % rule_group in the config.
    RulesGroupConfig =
        [
            #{
                dirs => ["src"],
                filter => "*.erl",
                ruleset => erl_files
            },
            #{
                dirs => ["include"],
                filter => "*.erl",
                ruleset => hrl_files
            },
            #{
                dirs => ["_build/test/lib/elvis_core/ebin"],
                filter => "*.beam",
                ruleset => beam_files
            },
            #{
                dirs => ["."],
                filter => "rebar.config",
                ruleset => rebar_config
            },
            #{
                dirs => ["."],
                filter => "elvis.config",
                ruleset => elvis_config
            }
        ],
    ok =
        try
            ok = elvis_core:rock(RulesGroupConfig)
        catch
            {invalid_config, _} ->
                fail
        end,
    % Override default elvis_core rules without ruleset should fail.
    OverrideFailConfig =
        [
            #{
                dirs => ["src"],
                rules =>
                    [
                        {elvis_text_style, line_length, #{limit => 90}},
                        {elvis_style, state_record_and_type, disable}
                    ]
            }
        ],
    ok =
        try
            _ = elvis_core:rock(OverrideFailConfig),
            fail
        catch
            {invalid_config, _} ->
                ok
        end,
    % Override default elvis_core rules.
    OverrideConfig =
        [
            #{
                dirs => ["src"],
                filter => "*.erl",
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
                dirs => ["."],
                filter => "rebar.config",
                ruleset => rebar_config
            },
            #{
                dirs => ["."],
                filter => "elvis.config",
                ruleset => elvis_config
            }
        ],
    ok =
        try
            ok = elvis_core:rock(OverrideConfig)
        catch
            {invalid_config, _} ->
                fail
        end.

rock_this_skipping_files(_Config) ->
    meck:new(elvis_file, [passthrough]),
    Dirs = ["../../_build/test/lib/elvis_core/test/examples"],
    [File] = elvis_file:find_files(Dirs, "small.erl"),
    Path = elvis_file:path(File),
    ConfigPath = "../../config/elvis-test-pa.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    ok = elvis_core:rock_this(Path, ElvisConfig),
    0 = meck:num_calls(elvis_file, load_file_data, '_'),
    meck:unload(elvis_file),
    ok.

rock_this_not_skipping_files(_Config) ->
    meck:new(elvis_file, [passthrough]),
    Dirs = ["../../_build/test/lib/elvis_core/test/examples"],
    [File] = elvis_file:find_files(Dirs, "small.erl"),
    Path = elvis_file:path(File),
    ElvisConfig = elvis_test_utils:config(),
    ok = elvis_core:rock_this(Path, ElvisConfig),
    1 = meck:num_calls(elvis_file, load_file_data, '_'),
    meck:unload(elvis_file),
    ok.

rock_with_umbrella_apps(_Config) ->
    ElvisUmbrellaConfigFile = "../../config/elvis-umbrella.config",
    ElvisConfig = elvis_config:from_file(ElvisUmbrellaConfigFile),
    {fail, [
        #{file := "../../_build/test/lib/elvis_core/test/dirs/test/dir_test.erl"},
        #{file := "../../_build/test/lib/elvis_core/test/dirs/src/dirs_src.erl"},
        #{
            file :=
                "../../_build/test/lib/elvis_core/test/dirs/apps/app2/test/dirs_apps_app2_test.erl"
        },
        #{
            file :=
                "../../_build/test/lib/elvis_core/test/dirs/apps/app2/src/dirs_apps_app2_src.erl"
        },
        #{
            file :=
                "../../_build/test/lib/elvis_core/test/dirs/apps/app1/test/dirs_apps_app1_test.erl"
        },
        #{
            file :=
                "../../_build/test/lib/elvis_core/test/dirs/apps/app1/src/dirs_apps_app1_src.erl"
        }
    ]} =
        elvis_core:rock(ElvisConfig),
    ok.

rock_with_invalid_rules(_Config) ->
    ConfigPath = "../../test/examples/invalid_rules.elvis.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    ExpectedErrorMessage =
        {invalid_rules, [
            {invalid_rule, {elvis_style, not_existing_rule}},
            {invalid_rule, {elvis_style, what_is_this_rule}},
            {invalid_rule, {not_existing_module, dont_repeat_yourself}},
            {invalid_rule, {not_existing_module, dont_repeat_yourself}}
        ]},
    try
        ok = elvis_core:rock(ElvisConfig),
        ct:fail("Elvis should not have rocked with ~p", [ElvisConfig])
    catch
        {invalid_config, ExpectedErrorMessage} ->
            ok
    end.

%%%%%%%%%%%%%%%
%%% Utils

custom_ruleset(_Config) ->
    ConfigPath = "../../config/elvis-test-custom-ruleset.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    [[{elvis_text_style, no_tabs, #{}}]] = elvis_config:rules(ElvisConfig),

    %% read unknown ruleset configuration to ensure rulesets from
    %% previous load do not stick around
    ConfigPathMissing = "../../config/elvis-test-unknown-ruleset.config",
    ElvisConfigMissing = elvis_config:from_file(ConfigPathMissing),
    [[]] = elvis_config:rules(ElvisConfigMissing),
    ok.

hrl_ruleset(_Config) ->
    ConfigPath = "../../config/elvis-test-hrl-files.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    {fail, [
        #{file := "../../_build/test/lib/elvis_core/test/examples/test_good.hrl", rules := []},
        #{
            file := "../../_build/test/lib/elvis_core/test/examples/test_bad.hrl",
            rules := [#{name := line_length}]
        }
    ]} =
        elvis_core:rock(ElvisConfig),
    ok.

throw_configuration(_Config) ->
    Filename = "./elvis.config",
    ok = file:write_file(Filename, <<"-">>),
    ok =
        try
            _ = elvis_config:from_file(Filename),
            fail
        catch
            _ ->
                ok
        after
            file:delete(Filename)
        end.

find_file_and_check_src(_Config) ->
    Dirs = ["../../test/examples"],

    [] = elvis_file:find_files(Dirs, "doesnt_exist.erl"),
    [File] = elvis_file:find_files(Dirs, "small.erl"),

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
    Dirs = ["../../test/examples"],
    Filter = "find_test*.erl",
    Ignore = elvis_config:ignore(#{ignore => [find_test1]}),
    Files = [_, _] = elvis_file:find_files(Dirs, Filter),
    [_, _] = elvis_file:filter_files(Files, Dirs, Filter, []),
    [#{path := "../../test/examples/find_test2.erl"}] =
        elvis_file:filter_files(Files, Dirs, Filter, Ignore).

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

check_some_line_output(Fun, Expected, FilterFun) ->
    _ = ct:capture_start(),
    _ = Fun(),
    _ = ct:capture_stop(),
    Lines = ct:capture_get([]),
    ListFun = fun(Line) -> FilterFun(Line, Expected) end,
    [_ | _] = lists:filter(ListFun, Lines).

get_output(Fun) ->
    _ = ct:capture_start(),
    _ = Fun(),
    _ = ct:capture_stop(),
    ct:capture_get([]).

matches_regex(Result, Regex) ->
    case re:run(Result, Regex) of
        {match, _} ->
            true;
        nomatch ->
            false
    end.
