-module(project_SUITE).

-if(?OTP_RELEASE >= 23).

-behaviour(ct_suite).

-endif.

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([verify_no_branch_deps/1, verify_hex_dep/1, verify_git_for_deps/1,
         verify_protocol_for_deps/1, verify_old_config_format/1]).

-define(EXCLUDED_FUNS, [module_info, all, test, init_per_suite, end_per_suite]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(elvis_core),
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec verify_no_branch_deps(config()) -> any().
verify_no_branch_deps(_Config) ->
    ElvisConfig = elvis_test_utils:config(rebar_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar.config.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _, _] = elvis_project:no_branch_deps(ElvisConfig, File, #{}),

    RuleConfig = #{ignore => [jsx]},
    [_, _] = elvis_project:no_branch_deps(ElvisConfig, File, RuleConfig),

    RuleConfig1 = #{ignore => [jsx, getopt]},
    [] = elvis_project:no_branch_deps(ElvisConfig, File, RuleConfig1),

    RuleConfig2 = #{ignore => [getopt]},
    [_, _] = elvis_project:no_branch_deps(ElvisConfig, File, RuleConfig2).

-spec verify_git_for_deps(config()) -> any().
verify_git_for_deps(_Config) ->
    ElvisConfig = elvis_test_utils:config(rebar_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar.config.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _, _, _, _, _] = elvis_project:git_for_deps(ElvisConfig, File, #{}),

    RuleConfig = #{ignore => [getopt]},
    [_, _, _, _, _] = elvis_project:git_for_deps(ElvisConfig, File, RuleConfig),

    RuleConfig1 = #{ignore => [getopt, lager]},
    [_, _, _] = elvis_project:git_for_deps(ElvisConfig, File, RuleConfig1),

    RuleConfig2 = #{ignore => [meck, jsx], regex => "git@.*"},
    [_, _, _, _, _, _, _, _] = elvis_project:git_for_deps(ElvisConfig, File, RuleConfig2).

-spec verify_protocol_for_deps(config()) -> any().
verify_protocol_for_deps(_Config) ->
    ElvisConfig = elvis_test_utils:config(rebar_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar.config.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [_, _, _, _, _, _, _] = elvis_project:protocol_for_deps(ElvisConfig, File, #{}),

    RuleConfig = #{ignore => [getopt, jsx]},
    [_, _, _, _] = elvis_project:protocol_for_deps(ElvisConfig, File, RuleConfig),

    RuleConfig1 = #{ignore => [getopt, lager]},
    [_, _, _] = elvis_project:protocol_for_deps(ElvisConfig, File, RuleConfig1),

    RuleConfig2 = #{ignore => [meck], regex => "git@.*"},
    [_, _, _, _, _, _, _, _, _] =
        elvis_project:protocol_for_deps(ElvisConfig, File, RuleConfig2).

-spec verify_hex_dep(config()) -> any().
verify_hex_dep(_Config) ->
    ElvisConfig = elvis_test_utils:config(rebar_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar3.config.success",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    [] = elvis_project:protocol_for_deps(ElvisConfig, File, #{}).

-spec verify_old_config_format(config()) -> any().
verify_old_config_format(_Config) ->
    ElvisConfig = elvis_test_utils:config(elvis_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    PathFail = "fail.elvis.config",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),
    [_] = elvis_project:old_configuration_format(ElvisConfig, FileFail, #{}),

    PathFail1 = "fail.1.elvis.config",
    {ok, FileFail1} = elvis_test_utils:find_file(SrcDirs, PathFail1),
    [_] = elvis_project:old_configuration_format(ElvisConfig, FileFail1, #{}),

    PathFail2 = "fail.2.elvis.config",
    {ok, FileFail2} = elvis_test_utils:find_file(SrcDirs, PathFail2),
    [_] = elvis_project:old_configuration_format(ElvisConfig, FileFail2, #{}),

    PathPass = "pass.elvis.config",
    {ok, FilePass} = elvis_test_utils:find_file(SrcDirs, PathPass),
    [] = elvis_project:old_configuration_format(ElvisConfig, FilePass, #{}).
