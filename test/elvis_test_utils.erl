-module(elvis_test_utils).

-export([
    config/0,
    config_erl_files/1,
    config/1,
    find_file/2,
    elvis_core_apply_rule/5,
    excluded_funs_all/0,
    check_some_line_output/3,
    matches_regex/2
]).

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

config_erl_files(File) when is_atom(File) ->
    ModuleInfo = File:module_info(compile),
    Source = proplists:get_value(source, ModuleInfo),
    config_erl_files(Source);
config_erl_files(File) ->
    [
        #{
            files => [File],
            ruleset => erl_files
        }
    ].

config(Ruleset) ->
    RulesetCfgs = application:get_env(elvis_core, config, []),
    [Config] = [Cfg || #{ruleset := R} = Cfg <- RulesetCfgs, R =:= Ruleset],
    Config.

find_file(FileGlobs, Filename) ->
    find_file_loop(FileGlobs, Filename).

find_file_loop([Glob | Rest], Filename) ->
    Dir = filename:dirname(Glob),
    Path = filename:join(Dir, Filename),
    case filelib:wildcard(Path) of
        [Match | _] ->
            {ok, elvis_file:from_path(Match)};
        [] ->
            find_file_loop(Rest, Filename)
    end;
find_file_loop([], _Filename) ->
    {error, enoent}.

elvis_core_apply_rule(Config, Module, Function, RuleConfig, Filename) ->
    ElvisConfig =
        elvis_test_utils:config(
            proplists:get_value(group, Config, erl_files)
        ),
    FileGlobs = elvis_config:file_globs(ElvisConfig),
    {ok, File} = elvis_test_utils:find_file(FileGlobs, Filename),
    {[RulesResults], _, _} =
        elvis_core:apply_rule(elvis_rule:new(Module, Function, RuleConfig), {[], ElvisConfig, File}),
    case RulesResults of
        #{error_msg := Msg, info := Info} ->
            ct:fail(Msg, Info);
        #{items := Items} ->
            Items
    end.

check_some_line_output(Fun, Expected, FilterFun) ->
    _ = ct:capture_start(),
    _ = Fun(),
    _ = ct:capture_stop(),
    Lines = ct:capture_get([]),
    ListFun = fun(Line) -> FilterFun(Line, Expected) end,
    [_ | _] = lists:filter(ListFun, Lines).

matches_regex(Result, Regex) ->
    nomatch =/= re:run(Result, Regex).
