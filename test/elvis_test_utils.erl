-module(elvis_test_utils).

-export([ config/0
        , config/1
        , find_file/2
        ]).

-spec config() -> elvis_config:configs().
config() ->
  application:get_env(elvis_core, config, []).

-spec config(Ruleset :: atom()) -> elvis_config:configs().
config(Ruleset) ->
  RulesetCfgs = application:get_env(elvis_core, config, []),
  [Config] = [Cfg || #{ruleset := R} = Cfg <- RulesetCfgs, R =:= Ruleset],
  Config.

-spec find_file([string()], string()) ->
    {ok, elvis_file:file()} | {error, enoent}.
find_file(Dirs, Pattern) ->
    case elvis_file:find_files(Dirs, Pattern) of
        [] -> {error, enoent};
        [Path | _] -> {ok, Path}
    end.
