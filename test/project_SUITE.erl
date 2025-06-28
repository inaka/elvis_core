-module(project_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([
    verify_no_branch_deps/1,
    verify_hex_dep/1,
    verify_protocol_for_deps/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, elvis_test_utils:excluded_funs_all())].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(elvis_core),
    Config.

end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_no_branch_deps(_Config) ->
    ElvisConfig = elvis_test_utils:config(rebar_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar.config.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _, _, _] = elvis_project:no_branch_deps({elvis_project, ElvisConfig, File, #{}}),

    RuleConfig = #{ignore => [jsx]},
    [_, _, _] = elvis_project:no_branch_deps({elvis_project, ElvisConfig, File, RuleConfig}),

    RuleConfig1 = #{ignore => [jsx, getopt]},
    [_] = elvis_project:no_branch_deps({elvis_project, ElvisConfig, File, RuleConfig1}),

    RuleConfig2 = #{ignore => [getopt]},
    [_, _, _] = elvis_project:no_branch_deps({elvis_project, ElvisConfig, File, RuleConfig2}).

verify_protocol_for_deps(_Config) ->
    ElvisConfig = elvis_test_utils:config(rebar_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar.config.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [
        #{info := [lager, _]},
        #{info := [getopt, _]},
        #{info := [jiffy, _]},
        #{info := [jsx, _]},
        #{info := [lager, _]},
        #{info := [getopt, _]},
        #{info := [jiffy, _]},
        #{info := [opentelemetry_api, _]}
    ] =
        elvis_project:protocol_for_deps({elvis_project, ElvisConfig, File, #{}}),

    RuleConfig = #{ignore => [getopt, jsx]},
    [_, _, _, _, _] = elvis_project:protocol_for_deps(
        {elvis_project, ElvisConfig, File, RuleConfig}
    ),

    RuleConfig1 = #{ignore => [getopt, lager]},
    [_, _, _, _] = elvis_project:protocol_for_deps({elvis_project, ElvisConfig, File, RuleConfig1}),

    RuleConfig2 = #{ignore => [meck], regex => "git@.*"},
    [_, _, _, _, _, _, _, _, _, _] =
        elvis_project:protocol_for_deps({elvis_project, ElvisConfig, File, RuleConfig2}).

verify_hex_dep(_Config) ->
    ElvisConfig = elvis_test_utils:config(rebar_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename1 = "rebar3.config.success",
    {ok, File1} = elvis_test_utils:find_file(SrcDirs, Filename1),

    [] = elvis_project:protocol_for_deps({elvis_project, ElvisConfig, File1, #{}}),

    Filename2 = "rebar3_2.config.success",
    {ok, File2} = elvis_test_utils:find_file(SrcDirs, Filename2),

    [] = elvis_project:protocol_for_deps({elvis_project, ElvisConfig, File2, #{}}).
