-module(style_SUITE).

-behaviour(ct_suite).

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).
-export([
    verify_function_naming_convention/1,
    verify_variable_naming_convention/1,
    verify_macro_names_rule/1,
    verify_no_macros/1,
    verify_no_block_expressions/1,
    verify_operator_spaces/1,
    verify_no_space/1,
    verify_no_space_after_pound/1,
    verify_operator_spaces_latin1/1,
    verify_nesting_level/1,
    verify_god_modules/1,
    verify_no_if_expression/1,
    verify_invalid_dynamic_call/1,
    verify_used_ignored_variable/1,
    verify_no_behavior_info/1,
    verify_module_naming_convention/1,
    verify_state_record_and_type/1,
    verify_state_record_and_type_plus_export_used_types/1,
    verify_no_spec_with_records/1,
    verify_dont_repeat_yourself/1,
    verify_max_module_length/1,
    verify_max_anonymous_function_arity/1,
    verify_max_function_arity/1,
    verify_max_function_length/1,
    verify_max_function_clause_length/1,
    verify_no_debug_call/1,
    verify_no_common_caveats_call/1,
    verify_no_call/1,
    verify_no_nested_try_catch/1,
    verify_no_successive_maps/1,
    verify_atom_naming_convention/1,
    verify_no_throw/1,
    verify_no_dollar_space/1,
    verify_no_author/1,
    verify_no_import/1,
    verify_no_catch_expressions/1,
    verify_no_single_clause_case/1,
    verify_numeric_format/1,
    verify_behaviour_spelling/1,
    verify_always_shortcircuit/1,
    verify_consistent_generic_type/1,
    verify_no_types/1,
    verify_no_nested_hrls/1,
    verify_no_specs/1,
    verify_export_used_types/1,
    verify_consistent_variable_casing/1,
    verify_no_match_in_condition/1,
    verify_param_pattern_matching/1,
    verify_private_data_types/1,
    verify_no_init_lists/1,
    verify_ms_transform_included/1,
    verify_no_boolean_in_comparison/1,
    verify_no_operation_on_same_value/1,
    verify_no_receive_without_timeout/1,
    verify_no_single_match_maybe/1,
    verify_guard_operators/1
]).
%% -elvis attribute
-export([
    verify_elvis_attr_atom_naming_convention/1,
    verify_elvis_attr_numeric_format/1,
    verify_elvis_attr_dont_repeat_yourself/1,
    verify_elvis_attr_function_naming_convention/1,
    verify_elvis_attr_god_modules/1,
    verify_elvis_attr_invalid_dynamic_call/1,
    verify_elvis_attr_line_length/1,
    verify_elvis_attr_macro_names/1,
    verify_elvis_attr_max_anonymous_function_arity/1,
    verify_elvis_attr_max_function_arity/1,
    verify_elvis_attr_max_function_length/1,
    verify_elvis_attr_max_module_length/1,
    verify_elvis_attr_module_naming_convention/1,
    verify_elvis_attr_nesting_level/1,
    verify_elvis_attr_no_behavior_info/1,
    verify_elvis_attr_no_call/1,
    verify_elvis_attr_no_debug_call/1,
    verify_elvis_attr_no_if_expression/1,
    verify_elvis_attr_no_nested_try_catch/1,
    verify_elvis_attr_no_successive_maps/1,
    verify_elvis_attr_no_spec_with_records/1,
    verify_elvis_attr_no_tabs/1,
    verify_elvis_attr_no_trailing_whitespace/1,
    verify_elvis_attr_operator_spaces/1,
    verify_elvis_attr_state_record_and_type/1,
    verify_elvis_attr_used_ignored_variable/1,
    verify_elvis_attr_variable_naming_convention/1,
    verify_elvis_attr_behaviour_spelling/1,
    verify_elvis_attr_param_pattern_matching/1,
    verify_elvis_attr_private_data_types/1
]).

-if(?OTP_RELEASE >= 27).
-export([]).

-if(?OTP_RELEASE >= 28).
-export([
    verify_operator_spaces_otp28/1
]).
-endif.
-endif.
%% Non-rule
-export([results_are_ordered_by_line/1, oddities/1]).

-if(?OTP_RELEASE < 27).

%% The `verify_max_module_length_docs/3` test only runs on OTP >= 27 because
%% the `-moduledoc` and `-doc` attributes were introduced in OTP-27.
-hank([{unnecessary_function_arguments, [{verify_max_module_length_docs, 3}]}]).

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, elvis_test_utils:excluded_funs_all())] ++
        [{group, beam_files}].

groups() ->
    [
        {beam_files, [sequence], [
            verify_function_naming_convention,
            verify_variable_naming_convention,
            verify_consistent_variable_casing,
            verify_nesting_level,
            verify_god_modules,
            verify_no_if_expression,
            verify_invalid_dynamic_call,
            verify_used_ignored_variable,
            verify_no_behavior_info,
            verify_module_naming_convention,
            verify_state_record_and_type,
            verify_state_record_and_type_plus_export_used_types,
            verify_no_spec_with_records,
            verify_dont_repeat_yourself,
            verify_no_debug_call,
            verify_no_common_caveats_call,
            verify_no_call,
            verify_no_nested_try_catch,
            verify_no_successive_maps,
            verify_atom_naming_convention,
            verify_no_throw,
            verify_no_author,
            verify_no_import,
            verify_always_shortcircuit,
            verify_no_catch_expressions,
            verify_no_single_clause_case,
            verify_no_macros,
            verify_export_used_types,
            verify_max_anonymous_function_arity,
            verify_max_function_arity,
            % verify_no_match_in_condition,
            verify_behaviour_spelling,
            verify_param_pattern_matching,
            verify_private_data_types
        ]}
    ].

init_per_suite(Config) ->
    _ = application:ensure_all_started(elvis_core),
    Config.

end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

init_per_group(beam_files = Group, Config) ->
    [{test_file_ext, "beam"}, {group, Group} | Config];
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    proplists:delete(test_file_ext, proplists:delete(group, Config)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Rules

verify_function_naming_convention(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % pass
    PathPass = "pass_function_naming_convention." ++ Ext,
    #{regex := DefaultRegex} = elvis_style:default(function_naming_convention),

    RuleConfig = #{regex => DefaultRegex},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            function_naming_convention,
            RuleConfig,
            PathPass
        ),

    RuleConfig2 = #{regex => DefaultRegex, ignore => [fail_function_naming_convention]},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            function_naming_convention,
            RuleConfig2,
            PathPass
        ),

    % fail
    PathFail = "fail_function_naming_convention." ++ Ext,
    [
        _CamelCaseError,
        _ALL_CAPSError,
        _InitialCapError,
        _HyphenError,
        _PredError,
        _EmailError,
        _BeforeAfter
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            function_naming_convention,
            RuleConfig,
            PathFail
        ),

    RuleConfig3 =
        #{
            regex => DefaultRegex,
            ignore =>
                [
                    {fail_function_naming_convention, camelCase},
                    {fail_function_naming_convention, 'ALL_CAPS'},
                    {fail_function_naming_convention, 'Initial_cap'},
                    {fail_function_naming_convention, 'ok-for-lisp'},
                    {fail_function_naming_convention, 'no_predicates?'}
                ]
        },
    [_EmailError2, _BeforeAfter2] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            function_naming_convention,
            RuleConfig3,
            PathFail
        ),

    % ignored
    PathIgnored = "fail_function_naming_convention_ignored_function." ++ Ext,

    RuleConfig4 =
        #{
            regex => DefaultRegex,
            ignore =>
                [
                    {fail_function_naming_convention, camelCase},
                    {fail_function_naming_convention, 'ALL_CAPS'},
                    {fail_function_naming_convention, 'Initial_cap'},
                    {fail_function_naming_convention, 'ok-for-lisp'},
                    {fail_function_naming_convention, 'no_predicates?'},
                    {fail_function_naming_convention, user@location}
                ]
        },
    [_AnError] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            function_naming_convention,
            RuleConfig4,
            PathIgnored
        ),

    % forbidden
    PathForbidden = "forbidden_function_naming_convention." ++ Ext,
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            function_naming_convention,
            #{regex => DefaultRegex, forbidden_regex => "[0-9]"},
            PathForbidden
        ).

verify_variable_naming_convention(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    RuleConfig = #{regex => "^_?([A-Z][0-9a-zA-Z]*)$"},
    #{regex := DefaultRegex} = elvis_style:default(variable_naming_convention),

    PathPass = "pass_variable_naming_convention." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            variable_naming_convention,
            RuleConfig,
            PathPass
        ),

    PathFail = "fail_variable_naming_convention." ++ Ext,
    [
        _AtSign,
        _Underline_Word_Separator,
        _Bad_Ignored_Variable,
        _AtSignAgain,
        _Underline_Word_SeparatorAgain
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            variable_naming_convention,
            RuleConfig,
            PathFail
        ),

    % forbidden
    PathForbidden = "forbidden_variable_naming_convention." ++ Ext,
    [_, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            variable_naming_convention,
            #{regex => DefaultRegex, forbidden_regex => "[0-9]"},
            PathForbidden
        ).

verify_consistent_variable_casing(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),
    PathPass = "pass_consistent_variable_casing." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, variable_casing, #{}, PathPass
        ),

    PathFail = "fail_consistent_variable_casing." ++ Ext,
    [
        #{info := ["TypeVar", _, ["Typevar"]]},
        #{
            info :=
                [
                    "GeneralInconsistency",
                    _,
                    [
                        "GENERALInconsistencY",
                        "GENERALInconsistency",
                        "GeNeRaLiNcOnSiStEnCy",
                        "GeneralINCONSISTENCY"
                    ]
                ]
        },
        #{info := ["SpecVar", _, ["SPECVar"]]},
        #{info := ["FuncVar", _, ["FUNCVar"]]},
        #{info := ["FunVar", _, ["FunVAR"]]},
        #{info := ["IgnVar", _, ["IGNVar"]]}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, variable_casing, #{}, PathFail
        ).

verify_macro_names_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_macro_names." ++ Ext,

    [_, _, _, _, _, _] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, macro_naming_convention, #{}, Path
    ),

    [_, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            macro_naming_convention,
            #{regex => "^[A-Za-z_ ]+$"},
            Path
        ),

    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            macro_naming_convention,
            #{regex => "^[A-Za-z_ \-]+$"},
            Path
        ),

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            macro_naming_convention,
            #{regex => "^[A-Za-z_, \-]+$"},
            Path
        ),

    [_, _, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            macro_naming_convention,
            #{regex => "^POTENTIAL_BAD-NAME$"},
            Path
        ),

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            macro_naming_convention,
            #{ignore => [fail_macro_names]},
            Path
        ),

    % forbidden
    PathForbidden = "forbidden_macro_names." ++ Ext,
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            macro_naming_convention,
            #{regex => "^[A-Za-z_, \-]+$", forbidden_regex => "FORBIDDEN"},
            PathForbidden
        ).

verify_no_macros(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_no_macros." ++ Ext,
    FailRes = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_macros, #{}, PathFail),
    case Ext of
        "beam" ->
            % no macros on BEAM files
            [] = FailRes;
        _ ->
            [_] = FailRes
    end,

    PathPass = "pass_no_macros." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            no_macros,
            #{allow => ['ALLOWED_MACRO']},
            PathPass
        ).

verify_no_types(Config) ->
    PathFail = "fail_no_types.hrl",
    [#{line_num := 1}] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_types, #{}, PathFail
    ),

    PathPass = "pass_no_types.hrl",
    [] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_types, #{}, PathPass).

verify_no_nested_hrls(Config) ->
    PathFail = "fail_no_nested_hrls.hrl",
    [#{line_num := 1}, #{line_num := 2}] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_nested_hrls, #{}, PathFail
    ),

    PathPass = "pass_no_nested_hrls.hrl",
    [] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_nested_hrls, #{}, PathPass).

verify_no_specs(Config) ->
    PathFail = "fail_no_specs.hrl",
    [#{line_num := 3}] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_specs, #{}, PathFail
    ),

    PathPass = "pass_no_specs.hrl",
    [] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_specs, #{}, PathPass).

verify_no_block_expressions(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_block_expressions." ++ Ext,

    [#{line_num := 9}] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, no_block_expressions, #{}, Path
        ).

verify_operator_spaces(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_operator_spaces." ++ Ext,

    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, operator_spaces, #{rules => []}, Path
    ),

    RuleConfig = #{rules => [{right, ","}]},
    [_, _, _] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, operator_spaces, RuleConfig, Path
    ),

    AppendOptions = #{rules => [{right, "++"}, {left, "++"}]},
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, operator_spaces, AppendOptions, Path
    ),

    SumOperation = #{rules => [{right, "+"}, {left, "+"}]},
    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, operator_spaces, SumOperation, Path
        ),

    MinusOperation = #{rules => [{right, "-"}, {left, "-"}]},
    [_, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, operator_spaces, MinusOperation, Path
        ),

    Arrow = #{rules => [{left, "->"}]},
    [_, _] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, operator_spaces, Arrow, Path
    ),

    BarOptions = #{rules => [{right, "|"}, {left, "|"}]},
    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, operator_spaces, BarOptions, Path
        ),

    ComprehensionOperation = #{rules => [{right, "||"}, {left, "||"}]},
    [_, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, operator_spaces, ComprehensionOperation, Path
        ),

    DefaultOptions = #{},
    [
        #{info := [right, "," | _]},
        #{info := [right, "," | _]},
        #{info := [left, "++" | _]},
        #{info := [right, "," | _]},
        #{info := [left, "+" | _]},
        #{info := [right, "+" | _]},
        #{info := [right, "|" | _]},
        #{info := [left, "|" | _]},
        #{info := [right, "||" | _]},
        #{info := [left, "||" | _]},
        #{info := [right, ";" | _]},
        #{info := [right, ";" | _]},
        #{info := [right, ";" | _]},
        #{info := [right, "::" | _]},
        #{info := [left, "::" | _]},
        #{info := [right, "->" | _]},
        #{info := [left, "->" | _]},
        #{info := [left, "->" | _]},
        #{info := [right, "=" | _]},
        #{info := [left, "=" | _]},
        #{info := [right, "=" | _]},
        #{info := [left, "=" | _]},
        #{info := [right, "+" | _]},
        #{info := [left, "+" | _]},
        #{info := [right, "-" | _]},
        #{info := [left, "-" | _]},
        #{info := [right, "*" | _]},
        #{info := [left, "*" | _]},
        #{info := [right, "/" | _]},
        #{info := [left, "/" | _]},
        #{info := [right, "=" | _]},
        #{info := [left, "=" | _]},
        #{info := [right, "=>" | _]},
        #{info := [right, "=>" | _]},
        #{info := [left, "=>" | _]},
        #{info := [left, "=>" | _]},
        #{info := [right, "=<" | _]},
        #{info := [left, "=<" | _]},
        #{info := [right, "<" | _]},
        #{info := [left, "<" | _]},
        #{info := [right, ">" | _]},
        #{info := [left, ">" | _]},
        #{info := [right, ">=" | _]},
        #{info := [left, ">=" | _]},
        #{info := [right, "==" | _]},
        #{info := [left, "==" | _]},
        #{info := [right, "=:=" | _]},
        #{info := [left, "=:=" | _]},
        #{info := [right, "/=" | _]},
        #{info := [left, "/=" | _]},
        #{info := [right, "=/=" | _]},
        #{info := [left, "=/=" | _]},
        #{info := [right, "--" | _]},
        #{info := [left, "--" | _]},
        #{info := [right, "=>" | _]},
        #{info := [left, "=>" | _]},
        #{info := [right, ":=" | _]},
        #{info := [left, ":=" | _]},
        #{info := [left, "<-" | _]},
        #{info := [right, "||" | _]},
        #{info := [left, "||" | _]},
        #{info := [right, "<=" | _]},
        #{info := [left, "<=" | _]},
        #{info := [right, "||" | _]},
        #{info := [left, "||" | _]},
        #{info := [right, "|" | _]},
        #{info := [left, "|" | _]},
        #{info := [right, "!" | _]},
        #{info := [left, "!" | _]},
        #{info := [right, "?=" | _]},
        #{info := [left, "?=" | _]}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, operator_spaces, DefaultOptions, Path
        ).

-if(?OTP_RELEASE >= 28).
verify_operator_spaces_otp28(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_operator_spaces_otp28." ++ Ext,
    DefaultOptions = #{},
    [
        #{info := [right, "<:-" | _]},
        #{info := [left, "<:=" | _]},
        #{info := [right, "<:-" | _]},
        #{info := [left, "<:-" | _]},
        #{info := [right, "&&" | _]},
        #{info := [left, "&&" | _]}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, operator_spaces, DefaultOptions, Path
        ).
-endif.

verify_no_space(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path1 = "fail_no_space." ++ Ext,
    [
        #{info := [right, "("], line_num := 3},
        #{info := [left, ","], line_num := 20},
        #{info := [right, "("], line_num := 36},
        #{info := [right, "("], line_num := 52},
        #{info := [left, ")"], line_num := 52},
        #{info := [left, ","], line_num := 76},
        #{info := [left, ")"], line_num := 79},
        #{info := [left, ";"], line_num := 94},
        #{info := [right, "("], line_num := 109},
        #{info := [left, ")"], line_num := 109},
        #{info := [left, "."], line_num := 118},
        #{info := [right, "#"], line_num := 121},
        #{info := [right, "?"], line_num := 121},
        #{info := [left, "."], line_num := 121},
        #{info := [left, "."], line_num := 125},
        #{info := [left, "."], line_num := 129},
        #{info := [left, ";"], line_num := 133},
        #{info := [left, ";"], line_num := 136},
        #{info := [left, ":"], line_num := 142},
        #{info := [left, ":"], line_num := 146}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            no_space,
            elvis_style:default(no_space),
            Path1
        ).

verify_no_space_after_pound(Config) ->
    PathFail = "fail_no_space_after_pound.erl",
    [
        #{line_num := 5},
        #{line_num := 7},
        #{line_num := 8},
        #{line_num := 12},
        #{line_num := 14},
        #{line_num := 14},
        #{line_num := 15},
        #{line_num := 16},
        #{line_num := 16},
        #{line_num := 18},
        #{line_num := 20},
        #{line_num := 20},
        #{line_num := 21},
        #{line_num := 22}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, no_space_after_pound, #{}, PathFail
        ),

    PathPass = "pass_no_space_after_pound.erl",
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_space_after_pound, #{}, PathPass
    ),
    ok.

verify_operator_spaces_latin1(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_operator_spaces_latin1." ++ Ext,

    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, operator_spaces, #{rules => []}, Path
    ),

    AppendOptions = #{rules => [{right, "++"}, {left, "++"}]},
    [_, _] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, operator_spaces, AppendOptions, Path
    ).

verify_nesting_level(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_nesting_level." ++ Ext,

    _ =
        case Group of
            beam_files ->
                [
                    #{line_num := 9},
                    #{line_num := 12},
                    #{line_num := 23},
                    #{line_num := 39},
                    #{line_num := 69},
                    #{line_num := 108},
                    #{line_num := 153},
                    #{line_num := 170}
                ] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_deep_nesting, #{level => 3}, Path
                    );
            erl_files ->
                [
                    #{line_num := 11},
                    #{line_num := 18},
                    #{line_num := 30},
                    #{line_num := 45},
                    #{line_num := 78},
                    #{line_num := 120},
                    #{line_num := 166},
                    #{line_num := 182}
                ] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_deep_nesting, #{level => 3}, Path
                    )
        end,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            no_deep_nesting,
            #{ignore => [fail_nesting_level]},
            Path
        ).

verify_god_modules(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_god_modules." ++ Ext,
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_god_modules, #{limit => 25}, Path
    ),

    RuleConfig = #{limit => 25, ignore => [fail_god_modules]},
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_god_modules, RuleConfig, Path
    ).

verify_no_if_expression(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_if_expression." ++ Ext,
    _ =
        case Group of
            beam_files ->
                [#{line_num := 8}, #{line_num := 18}, #{line_num := 26}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_if_expression, #{}, Path
                    );
            erl_files ->
                [#{line_num := 11}, #{line_num := 22}, #{line_num := 31}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_if_expression, #{}, Path
                    )
        end.

verify_invalid_dynamic_call(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_invalid_dynamic_call." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_invalid_dynamic_calls, #{}, PathPass
    ),

    PathPass2 = "pass_invalid_dynamic_call_callback." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_invalid_dynamic_calls, #{}, PathPass2
    ),

    PathFail = "fail_invalid_dynamic_call." ++ Ext,
    _ =
        case Group of
            beam_files ->
                [
                    % variable_module_name_call
                    #{line_num := _},
                    % variable_function_name_call
                    #{line_num := _},
                    #{line_num := _},
                    % call_module_name_call
                    #{line_num := _},
                    % call_function_name_call
                    #{line_num := _},
                    #{line_num := _},
                    % macro_call
                    #{line_num := _}
                ] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_invalid_dynamic_calls, #{}, PathFail
                    );
            erl_files ->
                [
                    % variable_module_name_call
                    #{line_num := 18},
                    % variable_function_name_call
                    #{line_num := 21},
                    #{line_num := 22},
                    % macro_module_name_call
                    #{line_num := 25},
                    % macro_function_name_call
                    #{line_num := 28},
                    #{line_num := 29},
                    % call_module_name_call
                    #{line_num := 32},
                    % call_function_name_call
                    #{line_num := 35},
                    #{line_num := 36}
                ] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_invalid_dynamic_calls, #{}, PathFail
                    )
        end,

    RuleConfig = #{ignore => [fail_invalid_dynamic_call]},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, no_invalid_dynamic_calls, RuleConfig, PathFail
        ).

verify_used_ignored_variable(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_used_ignored_variable." ++ Ext,
    Path2 = "used_ignored_variable_in_macro." ++ Ext,
    _ =
        case Group of
            beam_files ->
                [#{line_num := _}, #{line_num := _}, #{line_num := _}, #{line_num := _}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_used_ignored_variables, #{}, Path
                    );
            erl_files ->
                [#{line_num := 23}, #{line_num := 26}, #{line_num := 30}, #{line_num := 30}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_used_ignored_variables, #{}, Path
                    ),
                [] = elvis_test_utils:elvis_core_apply_rule(
                    Config, elvis_style, no_used_ignored_variables, #{}, Path2
                )
        end,

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            no_used_ignored_variables,
            #{ignore => [fail_used_ignored_variable]},
            Path
        ).

verify_no_behavior_info(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_behavior_info." ++ Ext,
    _ =
        case Group of
            beam_files ->
                [#{line_num := 7}, #{line_num := 10}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_behavior_info, #{}, Path
                    );
            erl_files ->
                [#{line_num := 14}, #{line_num := 17}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_behavior_info, #{}, Path
                    )
        end.

verify_module_naming_convention(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    #{regex := DefaultRegex} = elvis_style:default(module_naming_convention),
    RuleConfig = #{regex => DefaultRegex, ignore => []},

    PathPass = "pass_module_naming_convention." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            module_naming_convention,
            RuleConfig,
            PathPass
        ),

    PathFail = "fail_module_naming_1_convention_1_." ++ Ext,
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            module_naming_convention,
            RuleConfig,
            PathFail
        ),

    RuleConfigIgnore = RuleConfig#{ignore => [fail_module_naming_1_convention_1_]},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            module_naming_convention,
            RuleConfigIgnore,
            PathFail
        ),

    % forbidden
    PathForbidden = "forbidden_module_naming_convention_12." ++ Ext,
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            module_naming_convention,
            #{regex => DefaultRegex, forbidden_regex => "[0-9]"},
            PathForbidden
        ).

verify_state_record_and_type(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_state_record_and_type." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, state_record_and_type, #{}, PathPass
    ),

    PathPassWithOpaque = "pass_state_record_and_type_opaque." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            state_record_and_type,
            #{},
            PathPassWithOpaque
        ),

    PathPassGenStateM = "pass_state_record_and_type_gen_statem." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, state_record_and_type, #{}, PathPassGenStateM
        ),

    PathFail = "fail_state_record_and_type." ++ Ext,
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, state_record_and_type, #{}, PathFail
    ),

    PathFail1 = "fail_state_type." ++ Ext,
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, state_record_and_type, #{}, PathFail1
    ),

    PathBehaviourFail = "fail_state_record_and_type_behaviour." ++ Ext,
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, state_record_and_type, #{}, PathBehaviourFail
        ),

    PathFailGenStateMType = "fail_state_record_and_type_gen_statem_type." ++ Ext,
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            state_record_and_type,
            #{},
            PathFailGenStateMType
        ),

    PathPassGenStateMState = "fail_state_record_and_type_gen_statem_state." ++ Ext,
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            state_record_and_type,
            #{},
            PathPassGenStateMState
        ).

verify_state_record_and_type_plus_export_used_types(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_state_record_and_type_plus_export_used_types." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, state_record_and_type, #{}, PathPass
    ),
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, export_used_types, #{}, PathPass
    ),

    PathPassGenStateM =
        "pass_state_record_and_type_plus_export_used_types_gen_statem." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, state_record_and_type, #{}, PathPassGenStateM
        ),
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, export_used_types, #{}, PathPassGenStateM
        ),

    PathFail = "fail_state_record_and_type_plus_export_used_types." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, state_record_and_type, #{}, PathFail
    ),
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, export_used_types, #{}, PathFail
    ).

verify_behaviour_spelling(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "british_behaviour_spelling." ++ Ext,
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            behaviour_spelling,
            #{spelling => behavior},
            PathFail
        ),
    PathFail1 = "american_behavior_spelling." ++ Ext,
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            behaviour_spelling,
            #{spelling => behaviour},
            PathFail1
        ),

    PathPass = "british_behaviour_spelling." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            behaviour_spelling,
            #{spelling => behaviour},
            PathPass
        ),
    PathPass1 = "american_behavior_spelling." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            behaviour_spelling,
            #{spelling => behavior},
            PathPass1
        ).

verify_param_pattern_matching(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathRight = "right_param_pattern_matching." ++ Ext,
    PathLeft = "left_param_pattern_matching." ++ Ext,
    [
        #{info := ["Simple" | _]},
        #{info := ["SimpleToo" | _]},
        #{info := ["The" | _]},
        #{info := ["TheToo" | _]},
        #{info := ["AsYoda" | _]},
        #{info := ["AsYodaToo" | _]}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            param_pattern_matching,
            #{side => left},
            PathRight
        ),
    [
        #{info := ["Simple" | _]},
        #{info := ["SimpleToo" | _]},
        #{info := ["Multiple" | _]},
        #{info := ["MultipleToo" | _]},
        #{info := ["TheSecond" | _]},
        #{info := ["TheSecondToo" | _]},
        #{info := ["But" | _]},
        #{info := ["ButToo" | _]}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            param_pattern_matching,
            #{side => right},
            PathLeft
        ),

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            param_pattern_matching,
            #{side => right},
            PathRight
        ),

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            param_pattern_matching,
            #{side => left},
            PathLeft
        ).

verify_consistent_generic_type(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "consistent_generic_type_term." ++ Ext,
    [_, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            generic_type,
            #{preferred_type => any},
            PathFail
        ),
    PathFail1 = "consistent_generic_type_any." ++ Ext,
    [_, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            generic_type,
            #{preferred_type => term},
            PathFail1
        ),
    PathFail2 = "consistent_generic_type_term_and_any." ++ Ext,
    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            generic_type,
            #{preferred_type => any},
            PathFail2
        ),
    PathFail3 = "consistent_generic_type_term_and_any." ++ Ext,
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            generic_type,
            #{preferred_type => term},
            PathFail3
        ),

    PathPass = "consistent_generic_type_term." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            generic_type,
            #{preferred_type => term},
            PathPass
        ),
    PathPass1 = "consistent_generic_type_any." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            generic_type,
            #{preferred_type => any},
            PathPass1
        ),
    PathPass2 = "consistent_generic_type_no_checks." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            generic_type,
            #{preferred_type => term},
            PathPass2
        ),
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            generic_type,
            #{preferred_type => any},
            PathPass2
        ).

verify_always_shortcircuit(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_always_shortcircuit." ++ Ext,
    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, always_shortcircuit, #{}, PathFail
        ),

    PathPass = "pass_always_shortcircuit." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, always_shortcircuit, #{}, PathPass
    ).

verify_no_spec_with_records(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_no_spec_with_records." ++ Ext,
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, no_spec_with_records, #{}, PathFail
        ),

    PathPass = "pass_no_spec_with_records." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_spec_with_records, #{}, PathPass
    ).

verify_dont_repeat_yourself(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_dont_repeat_yourself." ++ Ext,
    RuleConfig5 = #{min_complexity => 5},
    Res1 =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, dont_repeat_yourself, RuleConfig5, PathFail
        ),
    2 = length(Res1),

    RuleConfig9 = #{min_complexity => 9},
    Res2 =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, dont_repeat_yourself, RuleConfig9, PathFail
        ),
    1 = length(Res2),

    IgnoreRule = #{ignore => [fail_dont_repeat_yourself]},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, dont_repeat_yourself, IgnoreRule, PathFail
        ),

    PathPass = "pass_dont_repeat_yourself." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, dont_repeat_yourself, RuleConfig5, PathPass
        ).

verify_max_module_length(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_max_module_length." ++ Ext,

    CountAllRuleConfig =
        #{
            count_comments => true,
            count_whitespace => true,
            count_docs => true
        },

    ct:comment("Count whitespace, comment, and documentation lines"),
    RuleConfig = CountAllRuleConfig#{max_length => 18},

    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, max_module_length, RuleConfig, PathFail
    ),

    RuleConfig1 = CountAllRuleConfig#{max_length => 22},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_module_length, RuleConfig1, PathFail
        ),

    RuleConfig2 = CountAllRuleConfig#{max_length => 23},
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, max_module_length, RuleConfig2, PathFail
    ),

    ct:comment("Don't count whitespace lines"),
    WhitespaceRuleConfig = CountAllRuleConfig#{count_whitespace => false},

    RuleConfig3 = WhitespaceRuleConfig#{max_length => 12},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_module_length, RuleConfig3, PathFail
        ),

    RuleConfig4 = WhitespaceRuleConfig#{max_length => 13},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_module_length, RuleConfig4, PathFail
        ),

    RuleConfig5 = WhitespaceRuleConfig#{max_length => 14},
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, max_module_length, RuleConfig5, PathFail
    ),

    ct:comment("Don't count comment or whitespace lines"),
    NoCountRuleConfig = WhitespaceRuleConfig#{count_comments => false},

    RuleConfig6 = NoCountRuleConfig#{max_length => 10},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_module_length, RuleConfig6, PathFail
        ),

    RuleConfig7 = NoCountRuleConfig#{max_length => 11},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_module_length, RuleConfig7, PathFail
        ),

    RuleConfig8 = NoCountRuleConfig#{max_length => 12},
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, max_module_length, RuleConfig8, PathFail
    ),

    ok = verify_max_module_length_docs(PathFail, CountAllRuleConfig, Config),

    {comment, ""}.

%% The `verify_max_module_length_docs/3` test only runs on OTP >= 27 because
%% the `-moduledoc` and `-doc` attributes were introduced in OTP-27.
-if(?OTP_RELEASE >= 27).

verify_max_module_length_docs(PathFail, CountAllRuleConfig, Config) ->
    ct:comment("Don't count -moduledoc and -doc attributes"),
    DocsRuleConfig = CountAllRuleConfig#{count_docs => false},

    RuleConfig9 = DocsRuleConfig#{max_length => 9},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_module_length, RuleConfig9, PathFail
        ),

    RuleConfig10 = DocsRuleConfig#{max_length => 17},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_module_length, RuleConfig10, PathFail
        ),

    RuleConfig11 = DocsRuleConfig#{max_length => 18},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_module_length, RuleConfig11, PathFail
        ),

    ok.

-else.

verify_max_module_length_docs(_PathFail, _CountAllRuleConfig, _Config) ->
    ok.

-endif.

verify_max_function_arity(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_max_function_arity." ++ Ext,
    RuleConfig = #{max_arity => 8},

    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, max_function_arity, RuleConfig, PathPass
    ),

    %% This module has functions with 0, 1, 2, and 3 arguments
    PathFail = "fail_max_function_arity." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, max_function_arity, #{}, PathFail
    ),
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_arity,
            #{max_arity => 2},
            PathFail
        ),
    [_, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_arity,
            #{max_arity => 1},
            PathFail
        ),
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_arity,
            #{max_arity => 0},
            PathFail
        ),
    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_arity,
            #{max_arity => -1},
            PathFail
        ),

    PathNonExportedPass = "pass_max_non_exported_function_arity." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_arity,
            #{max_arity => 3, non_exported_max_arity => 9},
            PathNonExportedPass
        ),

    PathNonExportedFail = "fail_max_non_exported_function_arity." ++ Ext,
    [_, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_arity,
            #{max_arity => 1, non_exported_max_arity => 2},
            PathNonExportedFail
        ),

    [_, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_arity,
            #{max_arity => 3, non_exported_max_arity => same},
            PathNonExportedPass
        ),
    ok.

verify_max_anonymous_function_arity(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_max_anonymous_function_arity." ++ Ext,
    RuleConfig = #{max_arity => 3},

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_anonymous_function_arity,
            RuleConfig,
            PathPass
        ),

    %% This module has funs with 0, 1, 2, and 3 arguments
    PathFail = "fail_max_anonymous_function_arity." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_anonymous_function_arity, #{}, PathFail
        ),
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_anonymous_function_arity,
            #{max_arity => 2},
            PathFail
        ),
    [_, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_anonymous_function_arity,
            #{max_arity => 1},
            PathFail
        ),
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_anonymous_function_arity,
            #{max_arity => 0},
            PathFail
        ),
    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_anonymous_function_arity,
            #{max_arity => -1},
            PathFail
        ),

    ok.

verify_max_function_length(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_max_function_length." ++ Ext,
    ModuleFail = fail_max_function_length,

    CountAllRuleConfig = #{count_comments => true, count_whitespace => true},

    ct:comment("Count whitespace and comment lines"),
    RuleConfig = CountAllRuleConfig#{max_length => 4},
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig, PathFail
        ),

    RuleConfig1 = CountAllRuleConfig#{max_length => 9},
    [_, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig1, PathFail
        ),

    RuleConfig2 = CountAllRuleConfig#{max_length => 14},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig2, PathFail
        ),

    RuleConfig3 = CountAllRuleConfig#{max_length => 15},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig3, PathFail
        ),

    ct:comment("Don't count whitespace lines"),
    WhitespaceRuleConfig = CountAllRuleConfig#{count_whitespace => false},

    RuleConfig4 = WhitespaceRuleConfig#{max_length => 3},
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig4, PathFail
        ),

    RuleConfig5 = WhitespaceRuleConfig#{max_length => 7},
    [_, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig5, PathFail
        ),

    RuleConfig6 = WhitespaceRuleConfig#{max_length => 8},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig6, PathFail
        ),

    RuleConfig7 = WhitespaceRuleConfig#{max_length => 11},
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig7, PathFail
        ),

    RuleConfig8 = WhitespaceRuleConfig#{max_length => 12},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig8, PathFail
        ),

    ct:comment("Don't count comment or whitespace lines"),
    NoCountRuleConfig = WhitespaceRuleConfig#{count_comments => false},

    RuleConfig9 = NoCountRuleConfig#{max_length => 1},
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig9, PathFail
        ),

    RuleConfig10 = NoCountRuleConfig#{max_length => 2},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig10, PathFail
        ),

    IgnoredFunctions = [{ModuleFail, f15}, {ModuleFail, f10, 1}],
    RuleConfig11 = RuleConfig5#{ignore => IgnoredFunctions},
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, max_function_length, RuleConfig11, PathFail
        ),

    {comment, ""}.

verify_max_function_clause_length(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_max_function_clause_length." ++ Ext,

    CountAllRuleConfig = #{count_comments => true, count_whitespace => true},
    RuleConfig = CountAllRuleConfig#{max_length => 10},

    ct:comment("Count whitespace and comment lines"),
    [_, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_clause_length,
            RuleConfig,
            PathFail
        ),

    PathSuccess = "pass_max_function_clause_length." ++ Ext,

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_clause_length,
            RuleConfig,
            PathSuccess
        ),

    RuleConfig2 = CountAllRuleConfig#{max_length => 15},
    PathExtraSuccess = "fail_max_function_length." ++ Ext,

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_clause_length,
            RuleConfig2,
            PathExtraSuccess
        ),

    RuleConfig3 = CountAllRuleConfig#{max_length => 1},

    [_, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_clause_length,
            RuleConfig3,
            PathFail
        ),

    RuleConfig4 = CountAllRuleConfig#{max_length => 1},
    PathClauseNumbers = "function_clause_numbers." ++ Ext,

    Result =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            max_function_clause_length,
            RuleConfig4,
            PathClauseNumbers
        ),

    Numbers = [Number || #{info := [Number, _, _, _]} <- Result],

    [
        "1st",
        "5th",
        "6th",
        "7th",
        "11th",
        "13th",
        "19th",
        "20th",
        "21st",
        "29th",
        "30th",
        "31st",
        "33rd"
    ] =
        Numbers.

verify_no_debug_call(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_no_debug_call." ++ Ext,

    _ =
        case Group of
            beam_files ->
                [
                    #{info := [erlang, display, 1], line_num := 8},
                    #{info := [io, format, 1], line_num := 9},
                    % ?DBG is preprocessed
                    #{info := [io, format, 2], line_num := 10},
                    #{info := [ct, print, 1], line_num := 16},
                    #{info := [ct, print, 2], line_num := 17},
                    #{info := [io, put_chars, 1], line_num := 18},
                    #{info := [dbg, whatever_function, 0], line_num := 19},
                    #{info := [dyntrace, calls, 1], line_num := 20},
                    #{info := [instrument, this, 0], line_num := 21}
                ] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_debug_call, #{}, PathFail
                    );
            erl_files ->
                [
                    #{info := [erlang, display, 1], line_num := 8},
                    #{info := [io, format, 1], line_num := 9},
                    #{info := [ct, pal, 1], line_num := 13},
                    #{info := [ct, pal, 2], line_num := 14},
                    #{info := [ct, print, 1], line_num := 15},
                    #{info := [ct, print, 2], line_num := 16},
                    #{info := [io, put_chars, 1], line_num := 17},
                    #{info := [dbg, whatever_function, 0], line_num := 18},
                    #{info := [dyntrace, calls, 1], line_num := 19},
                    #{info := [instrument, this, 0], line_num := 20}
                ] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_debug_call, #{}, PathFail
                    )
        end,

    RuleConfig = #{ignore => [fail_no_debug_call]},
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_debug_call, RuleConfig, PathFail
    ),

    RuleConfig2 = #{debug_functions => [{ct, pal, 2}]},
    _ =
        case Group of
            % ct:pal is preprocessed
            beam_files ->
                [] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config,
                        elvis_style,
                        no_debug_call,
                        RuleConfig2,
                        PathFail
                    );
            erl_files ->
                [_] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_debug_call, RuleConfig2, PathFail
                    )
        end,

    RuleConfig3 = #{debug_functions => [{ct, pal}]},
    _ =
        case Group of
            % ct:pal is preprocessed
            beam_files ->
                [] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config,
                        elvis_style,
                        no_debug_call,
                        RuleConfig3,
                        PathFail
                    );
            erl_files ->
                [_, _] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_debug_call, RuleConfig3, PathFail
                    )
        end,

    RuleConfig4 = #{debug_functions => [{io, format}]},
    _ =
        case Group of
            beam_files ->
                % pre-processing surfaces further issues with no_debug_call
                [_, _, _] = elvis_test_utils:elvis_core_apply_rule(
                    Config, elvis_style, no_debug_call, RuleConfig4, PathFail
                );
            erl_files ->
                [_, _] = elvis_test_utils:elvis_core_apply_rule(
                    Config, elvis_style, no_debug_call, RuleConfig4, PathFail
                )
        end,

    RuleConfig5 = #{debug_functions => [{ct, print}]},
    [_, _] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_debug_call, RuleConfig5, PathFail
    ).

%% We test no_call and no_common_caveats_call by building the equivalent config and make sure that
%% other than defaults, they behave the same
verify_no_common_caveats_call(Config) ->
    verify_no_call_flavours(Config, no_common_caveats_call, caveat_functions, 12).

verify_no_call(Config) ->
    verify_no_call_flavours(Config, no_call, no_call_functions, 0).

verify_no_call_flavours(
    Config,
    RuleName,
    RuleConfigMapKey,
    ExpectedDefaultRuleMatchCount
) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_no_call_classes." ++ Ext,

    assert_length(
        ExpectedDefaultRuleMatchCount,
        elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, RuleName, #{}, PathFail),
        RuleName
    ),

    RuleConfig = #{ignore => [fail_no_call_classes]},
    assert_length(
        0,
        elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, RuleName, RuleConfig, PathFail),
        RuleName
    ),

    RuleMatchTuples =
        [
            {{timer, send_after, 2}, 1},
            {{timer, send_after, 3}, 1},
            {{timer, send_interval, 2}, 1},
            {{timer, send_interval, 3}, 1},
            {{erlang, size, 1}, 2},
            {{timer, send_after}, 2},
            {{timer, '_', '_'}, 4},
            {{'_', tuple_size, 1}, 1},
            {{gen_statem, call, 2}, 1},
            {{gen_server, call, 2}, 1},
            {{gen_event, call, 3}, 1},
            {{erlang, list_to_atom, 1}, 1},
            {{erlang, binary_to_atom, 1}, 1},
            {{erlang, binary_to_atom, 2}, 1}
        ],

    lists:foreach(
        fun({FunSpec, ExpectedCount}) ->
            ThisRuleConfig = maps:from_list([{RuleConfigMapKey, [FunSpec]}]),
            Result =
                elvis_test_utils:elvis_core_apply_rule(
                    Config,
                    elvis_style,
                    RuleName,
                    ThisRuleConfig,
                    PathFail
                ),
            assert_length(ExpectedCount, Result, RuleName)
        end,
        RuleMatchTuples
    ).

verify_no_nested_try_catch(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Module = fail_no_nested_try_catch,
    Path = atom_to_list(Module) ++ "." ++ Ext,
    _ =
        case Group of
            beam_files ->
                [#{line_num := 9}, #{line_num := 18}, #{line_num := 21}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_nested_try_catch, #{}, Path
                    );
            erl_files ->
                [#{line_num := 15}, #{line_num := 30}, #{line_num := 37}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_nested_try_catch, #{}, Path
                    )
        end,

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            no_nested_try_catch,
            #{ignore => [Module]},
            Path
        ),

    Module2 = pass_no_nested_try_catch,
    Path2 = atom_to_list(Module2) ++ "." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_nested_try_catch, #{}, Path2
    ).

-if(?OTP_RELEASE < 27).

verify_no_successive_maps(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Module = fail_no_successive_maps,
    Path = atom_to_list(Module) ++ "." ++ Ext,

    Path2 = "fail_no_successive_maps2." ++ Ext,
    _ =
        case Group of
            beam_files ->
                [_, _, _] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_successive_maps, #{}, Path
                    ),
                [_, _, _] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_successive_maps, #{}, Path2
                    );
            erl_files ->
                [#{line_num := 7}, #{line_num := 8}, #{line_num := 9}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_successive_maps, #{}, Path
                    ),
                [#{line_num := 11}, #{line_num := 27}, #{line_num := 33}] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config, elvis_style, no_successive_maps, #{}, Path2
                    )
        end,

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            no_successive_maps,
            #{ignore => [Module]},
            Path
        ).

-else.

verify_no_successive_maps(_Config) ->
    [].

-endif.

verify_ms_transform_included(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_ms_transform_included." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, ms_transform_included, #{}, PassPath
    ),

    CustomFunctionPath = "custom_ms_transform_included." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            ms_transform_included,
            #{},
            CustomFunctionPath
        ),

    IncludedButNotUsed = "included_but_not_used_ms_transform." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            ms_transform_included,
            #{},
            IncludedButNotUsed
        ),

    DoubleInclude = "double_include_ms_transform." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, ms_transform_included, #{}, DoubleInclude
        ),

    FailPath = "fail_ms_transform_included." ++ Ext,
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, ms_transform_included, #{}, FailPath
    ),
    ok.

verify_no_operation_on_same_value(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_no_operation_on_same_value." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_operation_on_same_value, #{}, PassPath
    ),

    FailPath = "fail_no_operation_on_same_value." ++ Ext,
    [
        #{line_num := 9},
        #{line_num := 10},
        #{line_num := 11},
        #{line_num := 12},
        #{line_num := 13},
        #{line_num := 14},
        #{line_num := 15},
        #{line_num := 16},
        #{line_num := 17},
        #{line_num := 18},
        #{line_num := 19},
        #{line_num := 20},
        #{line_num := 21},
        #{line_num := 27},
        #{line_num := 33},
        #{line_num := 34},
        #{line_num := 39}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, no_operation_on_same_value, #{}, FailPath
        ),

    [
        #{line_num := 27},
        #{line_num := 28}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, no_operation_on_same_value, #{operations => ['--', '++']}, FailPath
        ).

verify_no_boolean_in_comparison(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_no_boolean_in_comparison." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_boolean_in_comparison, #{}, PassPath
    ),

    FailPath = "fail_no_boolean_in_comparison." ++ Ext,
    [
        #{line_num := 6},
        #{line_num := 6},
        #{line_num := 13},
        #{line_num := 16},
        % =:=
        #{line_num := 19},
        % =/=
        #{line_num := 22},
        #{line_num := 25},
        #{line_num := 28},
        #{line_num := 28},
        #{line_num := 32},
        #{line_num := 32}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, no_boolean_in_comparison, #{}, FailPath
        ).

verify_no_receive_without_timeout(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_no_receive_without_timeout." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_receive_without_timeout, #{}, PassPath
    ),

    FailPath = "fail_no_receive_without_timeout." ++ Ext,
    [
        #{line_num := 6},
        #{line_num := 10},
        #{line_num := 20}
    ] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, no_receive_without_timeout, #{}, FailPath
        ).

verify_atom_naming_convention(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    #{regex := DefaultRegex} = elvis_style:default(atom_naming_convention),

    BaseRegex = "^[a-z](_?[a-z0-9]+)*(_SUITE)?$",

    % pass
    PassModule = pass_atom_naming_convention,
    PassPath = atom_to_list(PassModule) ++ "." ++ Ext,
    PassModule2 = pass_atom_naming_convention_exception_class,
    PassPath2 = atom_to_list(PassModule2) ++ "." ++ Ext,
    PassModule3 = pass_maybe,
    PassPath3 = atom_to_list(PassModule3) ++ "." ++ Ext,

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => BaseRegex},
            PassPath
        ),
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => "^[^xwyhr]*$"},
            PassPath2
        ),
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => "^^[a-z]([a-zA-Z0-9@]*_?)*(_SUITE)?$"},
            PassPath3
        ),

    % fail
    FailModule = fail_atom_naming_convention,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,
    FailModule2 = fail_atom_naming_convention_exception_class,
    FailPath2 = atom_to_list(FailModule2) ++ "." ++ Ext,

    [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => BaseRegex, enclosed_atoms => same},
            FailPath
        ),
    [_, _, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => "^([a-zA-Z_]+)$", enclosed_atoms => same},
            FailPath
        ),
    [_, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => "^([a-zA-Z_' \\\\]+)$", enclosed_atoms => same},
            FailPath
        ),
    [_, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => "^([a-zA-Z\-_]+)$", enclosed_atoms => same},
            FailPath
        ),
    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => "^([a-zA-Z\-_' \\\\]+)$", enclosed_atoms => same},
            FailPath
        ),
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => "^([0-9]?[a-zA-Z\-_]+)$"},
            FailPath
        ),
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => BaseRegex, ignore => [FailModule]},
            FailPath
        ),
    KeepRegex = "^([a-zA-Z0-9_]+)$",
    [_, _, _, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => KeepRegex, enclosed_atoms => "^([a-z][a-z0-9A-Z_]*)$"},
            FailPath
        ),
    [_, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{
                regex => KeepRegex,
                enclosed_atoms => "^([a-z][a-z0-9A-Z_' \\\\]*)$"
            },
            FailPath
        ),
    [_, _, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => KeepRegex, enclosed_atoms => "^([a-z][\-a-z0-9A-Z_]*)$"},
            FailPath
        ),
    [_, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{
                regex => KeepRegex,
                enclosed_atoms => "^([0-9a-z][\-a-z0-9A-Z_' \\\\]*)$"
            },
            FailPath
        ),
    [_] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{regex => "^[^xwyhr]*$"},
            FailPath2
        ),
    _ =
        case Group of
            % 'or_THIS' getting stripped of enclosing '
            beam_files ->
                [_, _, _, _, _, _, _, _] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config,
                        elvis_style,
                        atom_naming_convention,
                        #{
                            regex => KeepRegex,
                            enclosed_atoms => "^([\\\\][\-a-z0-9A-Z_' \\\\]*)$"
                        },
                        FailPath
                    );
            erl_files ->
                [_, _, _, _, _, _, _, _, _] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config,
                        elvis_style,
                        atom_naming_convention,
                        #{
                            regex => KeepRegex,
                            enclosed_atoms => "^([\\\\][\-a-z0-9A-Z_' \\\\]*)$"
                        },
                        FailPath
                    )
        end,

    % forbidden
    PathForbidden = "forbidden_atom_naming_convention." ++ Ext,
    _ =
        case Group of
            % 'or_THIS' getting stripped of enclosing '
            beam_files ->
                [_, _, _, _] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config,
                        elvis_style,
                        atom_naming_convention,
                        #{regex => DefaultRegex, forbidden_regex => "[0-9]"},
                        PathForbidden
                    );
            erl_files ->
                [_, _, _] =
                    elvis_test_utils:elvis_core_apply_rule(
                        Config,
                        elvis_style,
                        atom_naming_convention,
                        #{regex => DefaultRegex, forbidden_regex => "[0-9]"},
                        PathForbidden
                    )
        end,

    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            atom_naming_convention,
            #{
                regex => DefaultRegex,
                forbidden_regex => "[0-9]",
                forbidden_enclosed_regex => same
            },
            PathForbidden
        ).

verify_no_init_lists(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    ExamplesDir = "no_init_lists_examples/",
    FailPath = ExamplesDir ++ "fail_no_init_lists." ++ Ext,
    FailPath2 = ExamplesDir ++ "fail_no_init_lists2." ++ Ext,
    FailPath3 = ExamplesDir ++ "fail_no_init_lists3." ++ Ext,
    FailPath4 = ExamplesDir ++ "fail_no_init_lists4." ++ Ext,
    FailPath5 = ExamplesDir ++ "fail_no_init_lists5." ++ Ext,
    FailPath6 = ExamplesDir ++ "fail_no_init_lists6." ++ Ext,
    FailPath7 = ExamplesDir ++ "fail_no_init_lists7." ++ Ext,
    FailPath8 = ExamplesDir ++ "fail_no_init_lists8." ++ Ext,

    [_] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_init_lists, #{}, FailPath),
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_init_lists, #{}, FailPath2
    ),
    [_, _, _] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_init_lists, #{}, FailPath3
    ),
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_init_lists, #{}, FailPath4
    ),
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_init_lists, #{}, FailPath5
    ),
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_init_lists, #{}, FailPath6
    ),
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_init_lists, #{}, FailPath7
    ),
    [_] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_init_lists, #{}, FailPath8
    ),

    PassPath = ExamplesDir ++ "pass_no_init_lists." ++ Ext,
    PassPath2 = ExamplesDir ++ "pass_no_init_lists2." ++ Ext,
    PassPath3 = ExamplesDir ++ "pass_no_init_lists3." ++ Ext,
    PassPath4 = ExamplesDir ++ "pass_no_init_lists4." ++ Ext,
    PassPath5 = ExamplesDir ++ "pass_no_init_lists5." ++ Ext,

    [] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_init_lists, #{}, PassPath),
    [] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_init_lists, #{}, PassPath2),
    [] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_init_lists, #{}, PassPath3),
    [] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_init_lists, #{}, PassPath4),
    [] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_init_lists, #{}, PassPath5),
    ok.

verify_no_throw(Config) ->
    _Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % fail
    FailModule = fail_no_throw,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    [_, _, _, _] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_throw, #{}, FailPath
    ).

verify_no_dollar_space(Config) ->
    _Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % fail
    FailModule = fail_no_dollar_space,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    [_, _] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_dollar_space, #{}, FailPath
    ).

verify_no_author(Config) ->
    _Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % fail
    FailModule = fail_no_author,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    [_, _] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_author, #{}, FailPath).

verify_no_import(Config) ->
    _Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    FailPath = "fail_no_import." ++ Ext,
    [_, _] = elvis_test_utils:elvis_core_apply_rule(Config, elvis_style, no_import, #{}, FailPath).

verify_no_catch_expressions(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    FailPath = "fail_no_catch_expressions." ++ Ext,

    R = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_catch_expressions, #{}, FailPath
    ),
    _ =
        case Group of
            beam_files ->
                [#{line_num := 10}, #{line_num := 21}, #{line_num := 21}] = lists:sort(R);
            erl_files ->
                [#{line_num := 9}, #{line_num := 24}, #{line_num := 26}] = lists:sort(R)
        end.

verify_no_single_clause_case(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_no_single_clause_case." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_single_clause_case, #{}, PassPath
    ),

    FailPath = "fail_no_single_clause_case." ++ Ext,

    R = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_single_clause_case, #{}, FailPath
    ),
    _ =
        case Group of
            beam_files ->
                [_, _, _] = R;
            erl_files ->
                [#{line_num := 6}, #{line_num := 14}, #{line_num := 16}] = R
        end.

verify_no_single_match_maybe(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_no_single_match_maybe." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_single_match_maybe, #{}, PassPath
    ),

    FailPath = "fail_no_single_match_maybe." ++ Ext,

    R = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_single_match_maybe, #{}, FailPath
    ),
    _ =
        case Group of
            beam_files ->
                [_, _, _] = R;
            erl_files ->
                [#{line_num := 8}, #{line_num := 16}, #{line_num := 17}] = R
        end.

%% erlfmt:ignore Lists of 40 elements are just too large to put each one on its own row
verify_guard_operators(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_guard_operators." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, guard_operators, #{}, PassPath
    ),

    FailPath = "fail_guard_operators." ++ Ext,
    Punctuation = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, guard_operators, #{preferred_syntax => punctuation}, FailPath
    ),
    Words = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, guard_operators, #{preferred_syntax => words}, FailPath
    ),
    PerExpression = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, guard_operators, #{preferred_syntax => per_expression}, FailPath
    ),
    PerGuard = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, guard_operators, #{preferred_syntax => per_clause}, FailPath
    ),
    Default = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, guard_operators, #{}, FailPath
    ),
    [
        #{line_num := 20},
        #{line_num := 21},
        #{line_num := 29},
        #{line_num := 30},
        #{line_num := 31},
        #{line_num := 41},
        #{line_num := 42},
        #{line_num := 61},
        #{line_num := 62},
        #{line_num := 74},
        #{line_num := 75},
        #{line_num := 83},
        #{line_num := 84},
        #{line_num := 85},
        #{line_num := 93},
        #{line_num := 94},
        #{line_num := 102},
        #{line_num := 103},
        #{line_num := 104},
        #{line_num := 112},
        #{line_num := 113},
        #{line_num := 121},
        #{line_num := 122},
        #{line_num := 123},
        #{line_num := 131},
        #{line_num := 132},
        #{line_num := 144},
        #{line_num := 145},
        #{line_num := 146},
        #{line_num := 158},
        #{line_num := 159},
        #{line_num := 171},
        #{line_num := 172},
        #{line_num := 173}
    ] = Punctuation,
    [
        #{line_num := 19},
        #{line_num := 21},
        #{line_num := 24},
        #{line_num := 25},
        #{line_num := 26},
        #{line_num := 40},
        #{line_num := 42},
        #{line_num := 47},
        #{line_num := 48},
        #{line_num := 49},
        #{line_num := 60},
        #{line_num := 62},
        #{line_num := 63},
        #{line_num := 64},
        #{line_num := 65},
        #{line_num := 73},
        #{line_num := 75},
        #{line_num := 78},
        #{line_num := 79},
        #{line_num := 80},
        #{line_num := 92},
        #{line_num := 94},
        #{line_num := 97},
        #{line_num := 98},
        #{line_num := 99},
        #{line_num := 111},
        #{line_num := 113},
        #{line_num := 116},
        #{line_num := 117},
        #{line_num := 118},
        #{line_num := 130},
        #{line_num := 132},
        #{line_num := 137},
        #{line_num := 138},
        #{line_num := 139},
        #{line_num := 157},
        #{line_num := 159},
        #{line_num := 164},
        #{line_num := 165},
        #{line_num := 166}
    ] = Words,
    true = Punctuation =/= Words,
    [
        #{line_num := 21},
        #{line_num := 42},
        #{line_num := 62},
        #{line_num := 75},
        #{line_num := 94},
        #{line_num := 113},
        #{line_num := 132},
        #{line_num := 159}
    ] = PerGuard,
    [
        #{line_num := 18},
        #{line_num := 39},
        #{line_num := 60},
        #{line_num := 72},
        #{line_num := 91},
        #{line_num := 110},
        #{line_num := 129},
        #{line_num := 154}
    ] = PerExpression,
    true = PerExpression =/= PerGuard,
    PerExpression = Default.

verify_no_match_in_condition(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_no_match_in_condition." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_match_in_condition, #{}, PassPath
    ),
    PassPath2 = "pass_no_match_in_condition2." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_match_in_condition, #{}, PassPath2
    ),
    PassPath3 = "pass_no_match_in_condition3." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_match_in_condition, #{}, PassPath3
    ),

    FailPath = "fail_no_match_in_condition." ++ Ext,
    R = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, no_match_in_condition, #{}, FailPath
    ),
    case Group of
        beam_files ->
            [_, _] = R;
        erl_files ->
            [#{line_num := 14}, #{line_num := 22}] = R
    end,
    ok.

verify_numeric_format(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    BaseRegex = "^[^_]+$",

    % pass
    PassModule = pass_numeric_format,
    PassPath = atom_to_list(PassModule) ++ "." ++ Ext,

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            numeric_format,
            #{regex => BaseRegex},
            PassPath
        ),

    % fail
    FailModule = fail_numeric_format,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    % no underscores
    [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            numeric_format,
            #{
                regex => BaseRegex,
                int_regex => same,
                float_regex => same
            },
            FailPath
        ),
    % with at least 2 digits
    [_, _, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            numeric_format,
            #{
                regex => "^(.*\\d\\d)$",
                int_regex => same,
                float_regex => same
            },
            FailPath
        ),
    % only base 10
    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            numeric_format,
            #{
                regex => "^([^#]+)$",
                int_regex => same,
                float_regex => same
            },
            FailPath
        ),

    % any float, nothing else - impossible to match base regex
    [_, _, _, _, _, _, _, _, _, _, _, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            numeric_format,
            #{
                regex => "impossible-to-match",
                int_regex => same,
                float_regex => ".*"
            },
            FailPath
        ),

    % any integer, nothing else - impossible to match base regex
    [_, _, _, _] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            numeric_format,
            #{
                regex => "impossible-to-match",
                int_regex => ".*",
                float_regex => same
            },
            FailPath
        ),

    % base regex is ignored
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            numeric_format,
            #{
                regex => "impossible-to-match",
                int_regex => ".*",
                float_regex => ".*"
            },
            FailPath
        ),

    % ignored module
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            numeric_format,
            #{regex => BaseRegex, ignore => [FailModule]},
            FailPath
        ),

    % ugly
    UglyModule = ugly_numeric_format,
    UglyPath = atom_to_list(UglyModule) ++ "." ++ Ext,

    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            numeric_format,
            #{regex => BaseRegex},
            UglyPath
        ),

    true.

verify_export_used_types(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),
    PathPass = "pass_export_used_types." ++ Ext,
    [] = elvis_test_utils:elvis_core_apply_rule(
        Config, elvis_style, export_used_types, #{}, PathPass
    ),

    PathFail = "fail_export_used_types." ++ Ext,
    [#{line_num := 3}] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, export_used_types, #{}, PathFail
        ).

verify_private_data_types(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),
    PathPass = "pass_private_data_types2." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            private_data_types,
            #{apply_to => [record, map, tuple]},
            PathPass
        ),
    PathPass2 = "pass_private_data_types2." ++ Ext,
    [] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            private_data_types,
            #{apply_to => [record, map, tuple]},
            PathPass2
        ),
    % Default applies only to records
    PathFail = "fail_private_data_types." ++ Ext,
    [#{line_num := _}] =
        elvis_test_utils:elvis_core_apply_rule(
            Config, elvis_style, private_data_types, #{}, PathFail
        ),
    [#{line_num := _}] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            private_data_types,
            #{apply_to => [tuple]},
            PathFail
        ),
    [#{line_num := _}] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            private_data_types,
            #{apply_to => [map]},
            PathFail
        ),
    [#{line_num := _}, #{line_num := _}, #{line_num := _}] =
        elvis_test_utils:elvis_core_apply_rule(
            Config,
            elvis_style,
            private_data_types,
            #{apply_to => [record, tuple, map]},
            PathFail
        ).

results_are_ordered_by_line(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    {fail, Results} = elvis_core:rock(ElvisConfig),
    true = lists:all(fun(X) -> X end, is_item_line_sort(Results)).

oddities(_Config) ->
    ElvisConfig =
        [
            #{
                dirs => ["../../_build/test/lib/elvis_core/test/examples"],
                filter => "odditiesß.erl",
                ruleset => erl_files,
                rules => [{elvis_style, no_god_modules, #{limit => 0}}]
            }
        ],
    {fail, [#{rules := [_, _, _, _]}]} = elvis_core:rock(ElvisConfig),
    true.

verify_elvis_attr_atom_naming_convention(Config) ->
    verify_elvis_attr(Config, "pass_atom_naming_convention_elvis_attr").

verify_elvis_attr_numeric_format(Config) ->
    verify_elvis_attr(Config, "pass_numeric_format_elvis_attr").

verify_elvis_attr_dont_repeat_yourself(Config) ->
    verify_elvis_attr(Config, "pass_dont_repeat_yourself_elvis_attr").

verify_elvis_attr_function_naming_convention(Config) ->
    verify_elvis_attr(Config, "pass_function_naming_convention_elvis_attr").

verify_elvis_attr_god_modules(Config) ->
    verify_elvis_attr(Config, "pass_god_modules_elvis_attr").

verify_elvis_attr_invalid_dynamic_call(Config) ->
    verify_elvis_attr(Config, "pass_invalid_dynamic_call_elvis_attr").

verify_elvis_attr_line_length(Config) ->
    verify_elvis_attr(Config, "pass_line_length_elvis_attr").

verify_elvis_attr_macro_names(Config) ->
    verify_elvis_attr(Config, "pass_macro_names_elvis_attr").

verify_elvis_attr_max_function_arity(Config) ->
    verify_elvis_attr(Config, "pass_max_function_arity_elvis_attr").

verify_elvis_attr_max_anonymous_function_arity(Config) ->
    verify_elvis_attr(Config, "pass_max_anonymous_function_arity_elvis_attr").

verify_elvis_attr_max_function_length(Config) ->
    verify_elvis_attr(Config, "pass_max_function_length_elvis_attr").

verify_elvis_attr_max_module_length(Config) ->
    verify_elvis_attr(Config, "pass_max_module_length_elvis_attr").

verify_elvis_attr_module_naming_convention(Config) ->
    verify_elvis_attr(Config, "pass_module_naming-convention_elvis_attr").

verify_elvis_attr_nesting_level(Config) ->
    verify_elvis_attr(Config, "pass_nesting_level_elvis_attr").

verify_elvis_attr_no_behavior_info(Config) ->
    verify_elvis_attr(Config, "pass_no_behavior_info_elvis_attr").

verify_elvis_attr_no_call(Config) ->
    verify_elvis_attr(Config, "pass_no_call_elvis_attr").

verify_elvis_attr_no_debug_call(Config) ->
    verify_elvis_attr(Config, "pass_no_debug_call_elvis_attr").

verify_elvis_attr_no_if_expression(Config) ->
    verify_elvis_attr(Config, "pass_no_if_expression_elvis_attr").

verify_elvis_attr_no_nested_try_catch(Config) ->
    verify_elvis_attr(Config, "pass_no_nested_try_catch_elvis_attr").

verify_elvis_attr_no_successive_maps(Config) ->
    verify_elvis_attr(Config, "pass_no_successive_maps_elvis_attr").

verify_elvis_attr_no_spec_with_records(Config) ->
    verify_elvis_attr(Config, "pass_no_spec_with_records_elvis_attr").

verify_elvis_attr_no_tabs(Config) ->
    verify_elvis_attr(Config, "pass_no_tabs_elvis_attr").

verify_elvis_attr_no_trailing_whitespace(Config) ->
    verify_elvis_attr(Config, "pass_no_trailing_whitespace_elvis_attr").

verify_elvis_attr_operator_spaces(Config) ->
    verify_elvis_attr(Config, "pass_operator_spaces_elvis_attr").

verify_elvis_attr_state_record_and_type(Config) ->
    verify_elvis_attr(Config, "pass_state_record_and_type_elvis_attr").

verify_elvis_attr_used_ignored_variable(Config) ->
    verify_elvis_attr(Config, "pass_used_ignored_variable_elvis_attr").

verify_elvis_attr_variable_naming_convention(Config) ->
    verify_elvis_attr(Config, "pass_variable_naming_convention_elvis_attr").

verify_elvis_attr_behaviour_spelling(Config) ->
    verify_elvis_attr(Config, "pass_behaviour_spelling_elvis_attr").

verify_elvis_attr_param_pattern_matching(Config) ->
    verify_elvis_attr(Config, "pass_param_pattern_matching_elvis_attr").

verify_elvis_attr_private_data_types(Config) ->
    verify_elvis_attr(Config, "pass_private_data_types_elvis_attr").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_elvis_attr(Config, FilenameNoExt) ->
    ElvisConfig =
        elvis_test_utils:config(
            proplists:get_value(group, Config, erl_files)
        ),
    SrcDirs = elvis_config:dirs(ElvisConfig),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    FullFilename = FilenameNoExt ++ "." ++ Ext,
    {ok, File} = elvis_test_utils:find_file(SrcDirs, FullFilename),

    ct:comment("Checking ~ts", [FullFilename]),
    {ok, #{rules := RuleResults}} = elvis_core:do_rock(File, ElvisConfig),
    [[] = Items || #{items := Items} <- RuleResults],
    {comment, ""}.

is_item_line_sort(Result) ->
    Items = [Items || #{rules := Rules} <- Result, #{items := Items} <- Rules],
    lists:map(fun is_list_sort/1, Items).

is_list_sort([_]) ->
    true;
is_list_sort([]) ->
    true;
is_list_sort([#{line_num := Line1} | T1]) ->
    [#{line_num := Line2} | _] = T1,
    case Line1 =< Line2 of
        true ->
            is_list_sort(T1);
        false ->
            false
    end.

assert_length(Expected, List, RuleName) ->
    case length(List) of
        Expected ->
            ok;
        _ ->
            throw({unexpected_response_length, RuleName, {expected, Expected}, {got, List}})
    end.
