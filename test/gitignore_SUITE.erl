-module(gitignore_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([verify_required_patterns/1, verify_forbidden_patterns/1]).

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

-spec verify_required_patterns(config()) -> any().
verify_required_patterns(_Config) ->
    GitIgnoreConfig = elvis_test_utils:config(gitignore),
    [SrcDirPass, SrcDirFail] = elvis_config:dirs(GitIgnoreConfig),
    NoRuleConfig = #{},

    PathPass = ".gitignore",
    {ok, FilePass} = elvis_test_utils:find_file([SrcDirPass], PathPass),
    {ok, []} = elvis_gitignore:required_patterns(GitIgnoreConfig, FilePass, NoRuleConfig),

    PathFail = ".gitignore",
    {ok, FileFail} = elvis_test_utils:find_file([SrcDirFail], PathFail),
    {ok, [Res]} = elvis_gitignore:required_patterns(GitIgnoreConfig, FileFail, NoRuleConfig),
    #{info := ["^doc/$"]} = Res.

-spec verify_forbidden_patterns(config()) -> any().
verify_forbidden_patterns(_Config) ->
    GitIgnoreConfig = elvis_test_utils:config(gitignore),
    [SrcDirPass, SrcDirFail] = elvis_config:dirs(GitIgnoreConfig),
    NoRuleConfig = #{},

    PathPass = ".gitignore",
    {ok, FilePass} = elvis_test_utils:find_file([SrcDirPass], PathPass),
    {ok, []} = elvis_gitignore:forbidden_patterns(GitIgnoreConfig, FilePass, NoRuleConfig),

    PathFail = ".gitignore",
    {ok, FileFail} = elvis_test_utils:find_file([SrcDirFail], PathFail),
    {ok, [Res]} = elvis_gitignore:forbidden_patterns(GitIgnoreConfig, FileFail, NoRuleConfig),
    #{info := ["^rebar.lock$"]} = Res.
