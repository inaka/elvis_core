-module(gitignore_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([verify_required_patterns/1, verify_forbidden_patterns/1]).

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

verify_required_patterns(_Config) ->
    GitIgnoreConfig = elvis_test_utils:config(gitignore),
    [SrcDirPass, SrcDirFail] = elvis_config:dirs(GitIgnoreConfig),
    NoRuleConfig = #{},

    PathPass = ".gitignore",
    {ok, FilePass} = elvis_test_utils:find_file([SrcDirPass], PathPass),
    [] = elvis_gitignore:required_patterns(GitIgnoreConfig, FilePass, NoRuleConfig),

    PathFail = ".gitignore",
    {ok, FileFail} = elvis_test_utils:find_file([SrcDirFail], PathFail),
    [Res] = elvis_gitignore:required_patterns(GitIgnoreConfig, FileFail, NoRuleConfig),
    #{info := ["^doc/$"]} = Res.

verify_forbidden_patterns(_Config) ->
    GitIgnoreConfig = elvis_test_utils:config(gitignore),
    [SrcDirPass, SrcDirFail] = elvis_config:dirs(GitIgnoreConfig),
    NoRuleConfig = #{},

    PathPass = ".gitignore",
    {ok, FilePass} = elvis_test_utils:find_file([SrcDirPass], PathPass),
    [] = elvis_gitignore:forbidden_patterns(GitIgnoreConfig, FilePass, NoRuleConfig),

    PathFail = ".gitignore",
    {ok, FileFail} = elvis_test_utils:find_file([SrcDirFail], PathFail),
    [Res] = elvis_gitignore:forbidden_patterns(GitIgnoreConfig, FileFail, NoRuleConfig),
    #{info := ["^rebar.lock$"]} = Res.
