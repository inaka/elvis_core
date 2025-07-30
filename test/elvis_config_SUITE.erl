-module(elvis_config_SUITE).

-behaviour(ct_suite).

% ct_suite
-export([all/0, init_per_suite/1, end_per_suite/1]).

% Tests
-export([rock_with_file_config/1]).
-export([rock_with_rebar_default_config/1]).
-export([throw_configuration/1]).

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
            exit:"elvis.config unconsultable: 1, erl_parse, [\"syntax error before: \",[]]" ->
                ok
        end,
    _ = file:delete(Filename).
