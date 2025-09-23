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

    Rule1 = elvis_rule:new(elvis_project, no_branch_deps, #{}),
    [_, _, _, _, _] = elvis_rule:execute(elvis_rule:file(Rule1, File), ElvisConfig),

    RuleConfig = #{ignore => [jsx]},
    Rule2 = elvis_rule:new(elvis_project, no_branch_deps, RuleConfig),
    [_, _, _] = elvis_rule:execute(elvis_rule:file(Rule2, File), ElvisConfig),

    RuleConfig1 = #{ignore => [jsx, getopt]},
    Rule3 = elvis_rule:new(elvis_project, no_branch_deps, RuleConfig1),
    [_] = elvis_rule:execute(elvis_rule:file(Rule3, File), ElvisConfig),

    RuleConfig2 = #{ignore => [getopt]},
    Rule4 = elvis_rule:new(elvis_project, no_branch_deps, RuleConfig2),
    [_, _, _] = elvis_rule:execute(elvis_rule:file(Rule4, File), ElvisConfig).

verify_protocol_for_deps(_Config) ->
    ElvisConfig = elvis_test_utils:config(rebar_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename = "rebar.config.fail",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),

    Rule1 = elvis_rule:new(elvis_project, protocol_for_deps, #{}),
    [
        #{info := [lager, _]},
        #{info := [getopt, _]},
        #{info := [jiffy, _]},
        #{info := [jsx, _]},
        #{info := [lager, _]},
        #{info := [getopt, _]},
        #{info := [jiffy, _]},
        #{info := [opentelemetry_api, _]}
    ] = elvis_rule:execute(elvis_rule:file(Rule1, File), ElvisConfig),

    RuleConfig = #{ignore => [getopt, jsx]},
    Rule2 = elvis_rule:new(elvis_project, protocol_for_deps, RuleConfig),
    [_, _, _, _, _] = elvis_rule:execute(elvis_rule:file(Rule2, File), ElvisConfig),

    RuleConfig1 = #{ignore => [getopt, lager]},
    Rule3 = elvis_rule:new(elvis_project, protocol_for_deps, RuleConfig1),
    [_, _, _, _] = elvis_rule:execute(elvis_rule:file(Rule3, File), ElvisConfig),

    RuleConfig2 = #{ignore => [meck], regex => "git@.*"},
    Rule4 = elvis_rule:new(elvis_project, protocol_for_deps, RuleConfig2),
    [_, _, _, _, _, _, _, _, _, _] = elvis_rule:execute(elvis_rule:file(Rule4, File), ElvisConfig).

verify_hex_dep(_Config) ->
    ElvisConfig = elvis_test_utils:config(rebar_config),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Filename1 = "rebar3.config.success",
    {ok, File1} = elvis_test_utils:find_file(SrcDirs, Filename1),

    Rule1 = elvis_rule:new(elvis_project, protocol_for_deps, #{}),
    [] = elvis_rule:execute(elvis_rule:file(Rule1, File1), ElvisConfig),

    Filename2 = "rebar3_2.config.success",
    {ok, File2} = elvis_test_utils:find_file(SrcDirs, Filename2),

    Rule2 = elvis_rule:new(elvis_project, protocol_for_deps, #{}),
    [] = elvis_rule:execute(elvis_rule:file(Rule2, File2), ElvisConfig).
