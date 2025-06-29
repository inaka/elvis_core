-module(elvis_test_utils).

-export([config/0, config/1, find_file/2, elvis_core_apply_rule/5, excluded_funs_all/0]).

excluded_funs_all() ->
    [
        module_info,
        all,
        groups,
        init_per_suite,
        end_per_suite,
        init_per_group,
        end_per_group
    ].

config() ->
    application:get_env(elvis_core, config, []).

config(Ruleset) ->
    RulesetCfgs = application:get_env(elvis_core, config, []),
    [Config] = [Cfg || #{ruleset := R} = Cfg <- RulesetCfgs, R =:= Ruleset],
    Config.

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
