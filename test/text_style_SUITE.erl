-module(text_style_SUITE).

-behaviour(ct_suite).

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    verify_line_length_rule/1,
    verify_line_length_rule_latin1/1,
    verify_unicode_line_length_rule/1,
    verify_no_tabs_rule/1,
    verify_no_trailing_whitespace_rule/1,
    verify_no_trailing_whitespace_rule_lf_crlf/1,
    verify_unquoted_atoms/1,
    verify_redundant_blank_lines/1
]).

all() ->
    [
        F
     || {F, _} <- ?MODULE:module_info(exports),
        not lists:member(F, elvis_test_utils:excluded_funs_all())
    ].

init_per_suite(Config) ->
    _ = application:ensure_all_started(elvis_core),
    Config.

end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

verify_line_length_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_line_length." ++ Ext,

    Result =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_text_style, line_length, #{limit => 100}, Path
        ),
    8 = length(Result),
    #{info := Info, message := Msg} = lists:nth(7, Result),
    <<"At line 32, there are too many ", _/binary>> = list_to_binary(io_lib:format(Msg, Info)),

    WholeLineResult =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_text_style,
            line_length,
            #{limit => 100, skip_comments => whole_line},
            Path
        ),
    6 = length(WholeLineResult),

    AnyResult =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_text_style,
            line_length,
            #{limit => 100, skip_comments => any},
            Path
        ),
    6 = length(AnyResult),

    WhistespaceResult =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_text_style,
            line_length,
            #{
                limit => 100,
                skip_comments => false,
                no_whitespace_after_limit => false
            },
            Path
        ),
    3 = length(WhistespaceResult).

verify_line_length_rule_latin1(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_line_length_latin1." ++ Ext,

    Result =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_text_style, line_length, #{limit => 100}, Path
        ),
    1 = length(Result),
    #{info := Info, message := Msg} = lists:nth(1, Result),
    <<"At line 13, there are too many ", _/binary>> = list_to_binary(io_lib:format(Msg, Info)).

verify_unicode_line_length_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "pass_unicode_comments." ++ Ext,

    Result =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_text_style, line_length, #{limit => 100}, Path
        ),
    0 = length(Result).

verify_no_tabs_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_tabs." ++ Ext,

    [_, _] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_text_style, no_tabs, #{}, Path).

verify_no_trailing_whitespace_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_trailing_whitespace." ++ Ext,

    do_verify_no_trailing_whitespace(Path, Config, #{ignore_empty_lines => true}, 3),
    do_verify_no_trailing_whitespace(Path, Config, #{ignore_empty_lines => false}, 4),
    do_verify_no_trailing_whitespace(Path, Config, #{}, 4).

verify_no_trailing_whitespace_rule_lf_crlf(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathCrLf = "pass_no_trailing_whitespace_crlf." ++ Ext,
    do_verify_no_trailing_whitespace(PathCrLf, Config, #{ignore_empty_lines => false}, 0),

    PathLf = "pass_no_trailing_whitespace_lf." ++ Ext,
    do_verify_no_trailing_whitespace(PathLf, Config, #{ignore_empty_lines => false}, 0).

do_verify_no_trailing_whitespace(Path, Config, RuleConfig, ExpectedNumItems) ->
    Items =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_text_style, no_trailing_whitespace, RuleConfig, Path
        ),
    length(Items) == ExpectedNumItems orelse
        ct:fail("Expected ~b error items. Got: ~p", [ExpectedNumItems, Items]).

verify_unquoted_atoms(Config) ->
    PassPath = "pass_unquoted_atoms." ++ "erl",
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, prefer_unquoted_atoms, #{}, PassPath
        ),

    FailPath = "fail_quoted_atoms." ++ "erl",
    [_, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, prefer_unquoted_atoms, #{}, FailPath
        ).

verify_redundant_blank_lines(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % pass
    PassModule = pass_redundant_blank_lines,
    PassPath = atom_to_list(PassModule) ++ "." ++ Ext,

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_text_style, no_redundant_blank_lines, #{}, PassPath
        ),

    % fail
    FailModule = fail_redundant_blank_lines,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_text_style, no_redundant_blank_lines, #{}, FailPath
        ).
