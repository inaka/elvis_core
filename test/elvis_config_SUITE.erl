-module(elvis_config_SUITE).

-behaviour(ct_suite).

% ct_suite
-export([all/0, init_per_suite/1, end_per_suite/1]).

% Tests
-export([rock_with_file_config/1]).
-export([rock_with_bananas/1]).
-export([rock_with_rebar_default_config/1]).
-export([throw_configuration/1]).
-export([validate_config_with_string_ignore/1]).
-export([validate/1]).

-include_lib("stdlib/include/assert.hrl").

% ct_suite
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, elvis_test_utils:excluded_funs_all())].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(elvis_core),
    Config.

end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

% Tests
rock_with_file_config(_Config) ->
    ConfigPath = "../../../../config/elvis.config",
    ElvisConfig = elvis_config:from_file(ConfigPath),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Expected =
        "# \\.\\./\\.\\./\\.\\./\\.\\./_build/test/lib/elvis_core/test/" ++
            "examples/.*\\.erl.*FAIL",
    [_ | _] = elvis_test_utils:check_some_line_output(
        Fun, Expected, fun elvis_test_utils:matches_regex/2
    ),
    ok.

rock_with_bananas(_Config) ->
    File = "../../../../_build/test/lib/elvis_core/test/examples/american_behavior_spelling.erl",
    ElvisConfig = [#{ files => [File], ruleset => erl_files }],

    elvis_config:set_warnings_as_errors(bananas),
    bananas = elvis_config:warnings_as_errors(),
    {errors, [
        #{
            file := File,
            rules := [#{name := behaviour_spelling}]
        }
    ]} = elvis_core:rock(ElvisConfig),

    elvis_config:set_warnings_as_errors(false),
    {warnings, [
        #{
            file := File,
            rules := [#{name := behaviour_spelling}]
        }
    ]} = elvis_core:rock(ElvisConfig),

    ok.

rock_with_rebar_default_config(_Config) ->
    {ok, _} = file:copy("../../../../config/rebar.config", "rebar.config"),
    ElvisConfig = elvis_config:from_rebar("rebar.config"),
    elvis_config:set_warnings_as_errors(false),
    [#{name := line_length}] =
        try
            {warnings, Results} = elvis_core:rock(ElvisConfig),
            [Rule || #{rules := [Rule]} <- Results]
        after
            file:delete("rebar.config")
        end,
    elvis_config:set_warnings_as_errors(true),
    ok.

throw_configuration(_Config) ->
    Filename = "./elvis.config",
    ok = file:write_file(Filename, <<"-">>),
    {error, "elvis.config unconsultable: 1, erl_parse, [\"syntax error before: \",[]]"} = elvis_config:from_file(
        Filename
    ),
    _ = file:delete(Filename).

%% @doc Regression test for https://github.com/inaka/elvis_core/issues/544
%%      String patterns (e.g. .hrl file paths) must be accepted in ignore lists.
validate_config_with_string_ignore(_Config) ->
    Config = [
        #{
            files => ["../../../../_build/test/lib/elvis_core/test/examples/*.erl"],
            ignore => ["include/file_that_i_want_to_ignore.hrl"],
            ruleset => erl_files
        }
    ],
    ok = elvis_config:validate_config(Config).

validate(_Config) ->
    ConfigDir = filename:join(["test", "examples", "configs"]),
    lists:foreach(
        fun({File, Expected0}) ->
            Expected = {error, Expected0},
            % In a normal use case scenario we either get an error and return
            % or call the function (at our own risk) several times in a row without
            % clearing state
            FilePath = filename:join(ConfigDir, File),
            FullPath = filename:join(["..", "..", "..", "..", FilePath]),
            case elvis_config:from_file(FullPath) of
                Expected ->
                    ok;
                {error, Comment} ->
                    % This is a minor hack, so the message is visible in the tests
                    ct:fail("Expected no failure from ~s; got -> ~s", [FilePath, Comment])
            end
        end,
        [
            % invalid top-level content (non-existing file)
            {"-1.config", "'elvis' is expected to exist and be a non-empty list."},

            % invalid elvis.config (unconsultable file)
            {"1_1.config",
                "elvis.config unconsultable: 1, erl_parse, [\"syntax error before: \",\"'.'\"]"},
            % invalid elvis (does not exist)
            {"1_2.config", "'elvis' is expected to exist and be a non-empty list."},
            % invalid elvis (is not a list)
            {"1_3.config", "'elvis' is expected to exist and be a non-empty list."},
            % invalid elvis (is an empty list)
            {"1_4.config", "'elvis' is expected to exist and be a non-empty list."},

            % invalid elvis.config (does not exist)
            {"2_1.config", "'elvis.config' is expected to exist and be a non-empty list."},
            % invalid elvis.config (is not a list)
            {"2_2.config", "'elvis.config' is expected to exist and be a non-empty list."},
            % invalid elvis.config (is an empty list)
            {"2_3.config", "'elvis.config' is expected to exist and be a non-empty list."},
            % invalid elvis.config element (is not a map)
            {"2_4.config",
                "in 'elvis.config', at list position number 1, element is expected to be a map."},
            % invalid elvis.config element (unknown key)
            {"2_5.config",
                "in 'elvis.config', at list position number 1, the following keys are unknown: [<<\"key\">>]."},

            % invalid config > files (does not exist)
            {"3_1.config",
                "in 'elvis.config', at list position number 1, 'files' is a compulsory option."},
            % invalid elvis.config > files (is not a list)
            {"3_2.config",
                "in 'elvis.config', at list position number 1, 'files' is expected to be a non-empty list."},
            % invalid elvis.config > files (is an empty list)
            {"3_3.config",
                "in 'elvis.config', at list position number 1, 'files' is expected to be a non-empty list."},

            % invalid config > rules + ruleset combo (one has to be defined)
            {"5_1.config",
                "in 'elvis.config', at list position number 1, either 'rules' is a non-empty list or 'ruleset' is defined."},
            % invalid elvis.config > rules (is not a list)
            {"5_2.config",
                "in 'elvis.config', at list position number 1, 'rules' is expected to be a list."},
            % invalid elvis.config > ruleset (is not valid)
            {"5_3.config",
                "in 'elvis.config', at list position number 1, 'not_a_ruleset' is expected to be either a custom or a default ruleset."},
            % invalid elvis.config > rules (is an empty list), ruleset is undefined
            {"5_4.config",
                "in 'elvis.config', at list position number 1, either 'rules' is a non-empty list or 'ruleset' is defined."},

            % invalid elvis (unknown key)
            {"6_1.config", "in 'elvis', the following keys are unknown: [<<\"key\">>]."},
            % invalid elvis.output_format (not "one of")
            {"6_2.config",
                "'elvis.output_format' is expected to be one of the following: [parsable, plain, colors]."},
            % invalid elvis.verbose (not a boolean)
            {"6_3.config", "'elvis.verbose' is expected to be a boolean."},
            % invalid elvis.no_output (not a boolean)
            {"6_4.config", "'elvis.no_output' is expected to be a boolean."},
            % invalid elvis.parallel (not an integer)
            {"6_5.config", "'elvis.parallel' is expected to be a positive integer."},
            % invalid elvis.parallel (not a positive integer)
            {"6_6.config", "'elvis.parallel' is expected to be a positive integer."}
        ]
    ).
