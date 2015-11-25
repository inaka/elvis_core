-module(elvis_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         %% Rocking
         rock_with_empty_map_config/1,
         rock_with_empty_list_config/1,
         rock_with_incomplete_config/1,
         rock_with_list_config/1,
         rock_with_file_config/1,
         rock_with_old_config/1,
         rock_this/1,
         rock_without_colors/1,
         %% Utill & Config
         throw_configuration/1,
         find_file_and_check_src/1,
         invalid_file/1,
         to_string/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = elvis_SUITE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    application:start(elvis),
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    application:stop(elvis),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Rocking

-spec rock_with_empty_map_config(config()) -> any().
rock_with_empty_map_config(_Config) ->
    ok = try
             elvis_core:rock(#{}),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end,
    ok = try
             elvis_core:rock([]),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end.

-spec rock_with_empty_list_config(config()) -> any().
rock_with_empty_list_config(_Config) ->
    ok = try
             elvis_core:rock([#{}, #{}]),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end.

-spec rock_with_incomplete_config(config()) -> any().
rock_with_incomplete_config(_Config) ->
    ElvisConfig = #{src_dirs => ["src"]},
    ok = try
             elvis_core:rock(ElvisConfig),
             fail
         catch
             throw:{invalid_config, _} -> ok
         end.

-spec rock_with_list_config(config()) -> any().
rock_with_list_config(_Config) ->
    ElvisConfig = [#{src_dirs => ["src"],
                     rules => []},
                   #{dirs => ["."],
                     filter => "Makefile",
                     rules => []}],
    ok = try
             elvis_core:rock(ElvisConfig),
             ok
         catch
             throw:{invalid_config, _} -> fail
         end.

-spec rock_with_file_config(config()) -> ok.
rock_with_file_config(_Config) ->
    Fun = fun() -> elvis_core:rock() end,
    Expected = "# \\.\\./\\.\\./test/examples/.*\\.erl.*FAIL",
    check_some_line_output(Fun, Expected, fun matches_regex/2),
    ok.

-spec rock_with_old_config(config()) -> ok.
rock_with_old_config(_Config) ->
    ConfigPath = "../../config/old/elvis.config",
    ElvisConfig = elvis_config:load_file(ConfigPath),
    ok = try
             elvis_core:rock(ElvisConfig),
             ok
         catch
             throw:{invalid_config, _} -> fail
         end,

    ConfigPath1 = "../../config/old/elvis-test.config",
    ElvisConfig1 = elvis_config:load_file(ConfigPath1),
    ok = try
             elvis_core:rock(ElvisConfig1),
             ok
         catch
             throw:{invalid_config, _} -> fail
         end,

    ConfigPath2 = "../../config/old/elvis-test-rule-config-list.config",
    ElvisConfig2 = elvis_config:load_file(ConfigPath2),
    ok = try
             elvis_core:rock(ElvisConfig2),
             ok
         catch
             throw:{invalid_config, _} -> fail
         end.

-spec rock_this(config()) -> ok.
rock_this(_Config) ->
    ok = elvis_core:rock_this(elvis_core),

    ok = try
             elvis_core:rock_this("bla.erl")
         catch
             _:{enoent, "bla.erl"} -> ok
         end,

    Path = "../../test/examples/fail_god_modules.erl",
    {fail, _} = elvis_core:rock_this(Path),

    ok.

-spec rock_without_colors(config()) -> ok.
rock_without_colors(_Config) ->
    ConfigPath = "../../config/test.config",
    ElvisConfig = elvis_config:load_file(ConfigPath),
    Fun = fun() -> elvis_core:rock(ElvisConfig) end,
    Expected = "\\e.*?m",
    ok = try check_some_line_output(Fun, Expected, fun matches_regex/2) of
             Result -> ct:fail("Unexpected result ~p", [Result])
         catch
             _:{badmatch, []} -> ok
         end.

%%%%%%%%%%%%%%%
%%% Utils

-spec throw_configuration(config()) -> any().
throw_configuration(_Config) ->
    Filename = "./elvis.config",
    ok = file:write_file(Filename, <<"-">>),
    ok = try
             elvis_config:default(),
             fail
         catch
             throw:_ -> ok
         after
             file:delete(Filename)
         end.

-spec find_file_and_check_src(config()) -> any().
find_file_and_check_src(_Config) ->
    Dirs = ["../../test/examples"],

    [] = elvis_file:find_files(Dirs, "doesnt_exist.erl"),
    [File] = elvis_file:find_files(Dirs, "small.erl"),

    {<<"-module(small).\n">>, _} = elvis_file:src(File),
    {error, enoent} = elvis_file:src(#{path => "doesnt_exist.erl"}).

-spec invalid_file(config()) -> any().
invalid_file(_Config) ->
    ok = try
             elvis_file:src(#{}),
             fail
         catch
             throw:{invalid_file, #{}} -> ok
         end.

-spec to_string(config()) -> any().
to_string(_Config) ->
    "1" = elvis_utils:to_str(1),
    "hello" = elvis_utils:to_str(<<"hello">>),
    "atom" = elvis_utils:to_str(atom).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_some_line_output(Fun, Expected, FilterFun) ->
    ct:capture_start(),
    Fun(),
    ct:capture_stop(),
    Lines = ct:capture_get([]),
    ListFun = fun(Line) -> FilterFun(Line, Expected) end,
    [_ | _] = lists:filter(ListFun, Lines).

matches_regex(Result, Regex) ->
    case re:run(Result, Regex) of
        {match, _} -> true;
        nomatch -> false
    end.
