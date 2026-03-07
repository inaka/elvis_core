-module(elvis_config_SUITE).

-behaviour(ct_suite).

% ct_suite
-export([all/0, init_per_suite/1, end_per_suite/1]).

% Tests
-export([rock_with_file_config/1]).
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

rock_with_rebar_default_config(_Config) ->
    {ok, _} = file:copy("../../../../config/rebar.config", "rebar.config"),
    ElvisConfig = elvis_config:from_rebar("rebar.config"),
    [#{name := line_length}] =
        try
            {fail, Results} = elvis_core:rock(ElvisConfig),
            [Rule || #{rules := [Rule]} <- Results]
        after
            file:delete("rebar.config")
        end,
    ok.

throw_configuration(_Config) ->
    Filename = "./elvis.config",
    ok = file:write_file(Filename, <<"-">>),
    ok =
        try
            elvis_config:from_file(Filename),
            fail
        catch
            exit:"elvis.config is unconsultable: 1, erl_parse, [\"syntax error before: \",[]]" ->
                ok
        end,
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
        fun({File, Expected}) ->
            % In a normal use case scenario we either get an error and return
            % or call the function (at our own risk) several times in a row without
            % clearing state
            FilePath = filename:join(ConfigDir, File),
            FullPath = filename:join(["..", "..", "..", "..", FilePath]),
            try elvis_config:from_file(FullPath) of
                Expected ->
                    ok;
                {fail, [{throw, {invalid_config, Comment}}]} ->
                    % This is a minor hack, so the message is visible in the tests
                    ct:fail("Expected no failure from ~s; got -> ~s", [FilePath, Comment])
            catch
                exit:Reason when Reason =:= Expected ->
                    ok
            end
        end,
        [
            % invalid top-level content (non-existing file)
            {"-1.config",
                invalid_config(
                    "'elvis.config' is expected to exist and be a non-empty list."
                )},

            % invalid elvis.config (unconsultable file)
            {"1_1.config",
                "elvis.config is unconsultable: 1, erl_parse, [\"syntax error before: \",\"'.'\"]"},

            % invalid config (does not exist)
            {"2_1.config",
                invalid_config(
                    "'config' is expected to exist and be a non-empty list."
                )},
            % invalid config (is not a list)
            {"2_2.config",
                invalid_config(
                    "'config' is expected to exist and be a non-empty list."
                )},
            % invalid config (is an empty list)
            {"2_3.config",
                invalid_config(
                    "'config' is expected to exist and be a non-empty list."
                )},
            % invalid config element (is not a map)
            {"2_4.config",
                invalid_config(
                    "in 'config', at list position number 1, element is expected to be a map."
                )},
            % invalid config element (unknown key)
            {"2_5.config",
                invalid_config(
                    "in 'config', at list position number 1, the following keys are unknown: [<<\"key\">>]."
                )},

            % invalid config > files (does not exist)
            {"3_1.config",
                invalid_config(
                    "in 'config', at list position number 1, 'files' is a compulsory option."
                )},
            % invalid config > files (is not a list)
            {"3_2.config",
                invalid_config(
                    "in 'config', at list position number 1, 'files' is expected to be a non-empty list."
                )},
            % invalid config > files (is an empty list)
            {"3_3.config",
                invalid_config(
                    "in 'config', at list position number 1, 'files' is expected to be a non-empty list."
                )},

            % invalid config > rules + ruleset combo (one has to be defined)
            {"5_1.config",
                invalid_config(
                    "in 'config', at list position number 1, either 'rules' is a non-empty list or 'ruleset' is defined."
                )},
            % invalid config > rules (is not a list)
            {"5_2.config",
                invalid_config(
                    "in 'config', at list position number 1, 'rules' is expected to be a list."
                )},
            % invalid config > ruleset (is not valid)
            {"5_3.config",
                invalid_config(
                    "in 'config', at list position number 1, 'not_a_ruleset' is expected to be either a custom or a default ruleset."
                )},
            % invalid config > rules (is an empty list), ruleset is undefined
            {"5_4.config",
                invalid_config(
                    "in 'config', at list position number 1, either 'rules' is a non-empty list or 'ruleset' is defined."
                )},

            % invalid (unknown key)
            {"6_1.config",
                invalid_config(
                    "in 'elvis.config', the following keys are unknown: [<<\"key\">>]."
                )},
            % invalid output_format (not "one of")
            {"6_2.config",
                invalid_config(
                    "'output_format' is expected to be one of the following: [parsable, plain, colors]."
                )},
            % invalid verbose (not a boolean)
            {"6_3.config",
                invalid_config(
                    "'verbose' is expected to be a boolean."
                )},
            % invalid no_output (not a boolean)
            {"6_4.config",
                invalid_config(
                    "'no_output' is expected to be a boolean."
                )},
            % invalid parallel (not an integer)
            {"6_5.config",
                invalid_config(
                    "'parallel' is expected to be a positive integer."
                )},
            % invalid parallel (not a positive integer)
            {"6_6.config",
                invalid_config(
                    "'parallel' is expected to be a positive integer."
                )}
        ]
    ).

invalid_config(Comment) ->
    {fail, [{throw, {invalid_config, Comment}}]}.
