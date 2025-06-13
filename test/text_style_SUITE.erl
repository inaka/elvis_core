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

-define(EXCLUDED_FUNS, [
    module_info,
    all,
    groups,
    test,
    init_per_suite,
    end_per_suite,
    init_per_group,
    end_per_group
]).

-type config() :: [{atom(), term()}].

-spec all() -> [atom()].
all() ->
    [F || {F, _} <- ?MODULE:module_info(exports), not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    _ = application:ensure_all_started(elvis_core),
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

-spec verify_line_length_rule(config()) -> any().
verify_line_length_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_line_length." ++ Ext,

    Result =
        style_SUITE:elvis_core_apply_rule(
            Config, elvis_text_style, line_length, #{limit => 100}, Path
        ),
    8 = length(Result),
    #{info := Info, message := Msg} = lists:nth(7, Result),
    <<"Line 32 is too long. It has ", _/binary>> = list_to_binary(io_lib:format(Msg, Info)),

    WholeLineResult =
        style_SUITE:elvis_core_apply_rule(
            Config,
            elvis_text_style,
            line_length,
            #{limit => 100, skip_comments => whole_line},
            Path
        ),
    6 = length(WholeLineResult),

    AnyResult =
        style_SUITE:elvis_core_apply_rule(
            Config,
            elvis_text_style,
            line_length,
            #{limit => 100, skip_comments => any},
            Path
        ),
    6 = length(AnyResult),

    WhistespaceResult =
        style_SUITE:elvis_core_apply_rule(
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

-spec verify_line_length_rule_latin1(config()) -> any().
verify_line_length_rule_latin1(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_line_length_latin1." ++ Ext,

    Result =
        style_SUITE:elvis_core_apply_rule(
            Config, elvis_text_style, line_length, #{limit => 100}, Path
        ),
    1 = length(Result),
    #{info := Info, message := Msg} = lists:nth(1, Result),
    <<"Line 13 is too long. It has", _/binary>> = list_to_binary(io_lib:format(Msg, Info)).

-spec verify_unicode_line_length_rule(config()) -> any().
verify_unicode_line_length_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "pass_unicode_comments." ++ Ext,

    Result =
        style_SUITE:elvis_core_apply_rule(
            Config, elvis_text_style, line_length, #{limit => 100}, Path
        ),
    0 = length(Result).

-spec verify_no_tabs_rule(config()) -> any().
verify_no_tabs_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_tabs." ++ Ext,

    [_, _] = style_SUITE:elvis_core_apply_rule(Config, elvis_text_style, no_tabs, #{}, Path).

-spec verify_no_trailing_whitespace_rule(config()) -> any().
verify_no_trailing_whitespace_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_trailing_whitespace." ++ Ext,

    do_verify_no_trailing_whitespace(Path, Config, #{ignore_empty_lines => true}, 3),
    do_verify_no_trailing_whitespace(Path, Config, #{ignore_empty_lines => false}, 4),
    do_verify_no_trailing_whitespace(Path, Config, #{}, 4).

-spec verify_no_trailing_whitespace_rule_lf_crlf(config()) -> any().
verify_no_trailing_whitespace_rule_lf_crlf(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathCrLf = "pass_no_trailing_whitespace_crlf." ++ Ext,
    do_verify_no_trailing_whitespace(PathCrLf, Config, #{ignore_empty_lines => false}, 0),

    PathLf = "pass_no_trailing_whitespace_lf." ++ Ext,
    do_verify_no_trailing_whitespace(PathLf, Config, #{ignore_empty_lines => false}, 0).

do_verify_no_trailing_whitespace(Path, Config, RuleConfig, ExpectedNumItems) ->
    Items =
        style_SUITE:elvis_core_apply_rule(
            Config, elvis_text_style, no_trailing_whitespace, RuleConfig, Path
        ),
    length(Items) == ExpectedNumItems orelse
        ct:fail("Expected ~b error items. Got: ~p", [ExpectedNumItems, Items]).

-spec verify_unquoted_atoms(config()) -> any().
verify_unquoted_atoms(Config) ->
    PassPath = "pass_unquoted_atoms." ++ "erl",
    [] =
        style_SUITE:elvis_core_apply_rule(
            Config, elvis_text_style, prefer_unquoted_atoms, #{}, PassPath
        ),

    FailPath = "fail_quoted_atoms." ++ "erl",
    [_, _] =
        style_SUITE:elvis_core_apply_rule(
            Config, elvis_text_style, prefer_unquoted_atoms, #{}, FailPath
        ).

-spec verify_redundant_blank_lines(config()) -> true.
verify_redundant_blank_lines(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % pass
    PassModule = pass_redundant_blank_lines,
    PassPath = atom_to_list(PassModule) ++ "." ++ Ext,

    [] =
        style_SUITE:elvis_core_apply_rule(
            Config, elvis_text_style, no_redundant_blank_lines, #{}, PassPath
        ),

    % fail
    FailModule = fail_redundant_blank_lines,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    [_, _, _] =
        style_SUITE:elvis_core_apply_rule(
            Config, elvis_text_style, no_redundant_blank_lines, #{}, FailPath
        ).
