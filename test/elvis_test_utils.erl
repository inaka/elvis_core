-module(elvis_test_utils).

-export([config/0, config/1, find_file/2, elvis_core_apply_rule/5]).

-spec config() -> elvis_config:configs().
config() ->
    application:get_env(elvis_core, config, []).

-spec config(RuleSet :: atom()) -> elvis_config:config().
config(RuleSet) ->
    RuleSetCfgs = application:get_env(elvis_core, config, []),
    [Config] = [Cfg || #{ruleset := R} = Cfg <- RuleSetCfgs, R =:= RuleSet],
    Config.

-spec find_file([string()], string()) -> {ok, elvis_file:file()} | {error, enoent}.
find_file(Dirs, Pattern) ->
    case elvis_file:find_files(Dirs, Pattern) of
        [] ->
            {error, enoent};
        [Path | _] ->
            {ok, Path}
    end.

elvis_core_apply_rule(Config, Module, Function, RuleConfig, Filename) ->
    ElvisConfig =
        elvis_test_utils:config(
            proplists:get_value(group, Config, erl_files)
        ),
    SrcDirs = elvis_config:dirs(ElvisConfig),
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Filename),
    {[RulesResults], _, _} =
        elvis_core:apply_rule({Module, Function, RuleConfig}, {[], ElvisConfig, File}),
    case RulesResults of
        #{error_msg := Msg, info := Info} ->
            ct:fail(Msg, Info);
        #{items := Items} ->
            Items
    end.
