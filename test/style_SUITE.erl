-module(style_SUITE).

-behaviour(ct_suite).

-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2,
         end_per_group/2]).
-export([verify_function_naming_convention/1, verify_variable_naming_convention/1,
         verify_line_length_rule/1, verify_line_length_rule_latin1/1,
         verify_unicode_line_length_rule/1, verify_no_tabs_rule/1,
         verify_no_trailing_whitespace_rule/1, verify_no_trailing_whitespace_rule_lf_crlf/1,
         verify_macro_names_rule/1, verify_macro_module_names/1, verify_no_macros/1,
         verify_no_block_expressions/1, verify_operator_spaces/1, verify_no_space/1,
         verify_no_space_after_pound/1, verify_operator_spaces_latin1/1, verify_nesting_level/1,
         verify_god_modules/1, verify_no_if_expression/1, verify_invalid_dynamic_call/1,
         verify_used_ignored_variable/1, verify_no_behavior_info/1,
         verify_module_naming_convention/1, verify_state_record_and_type/1,
         verify_state_record_and_type_plus_export_used_types/1, verify_no_spec_with_records/1,
         verify_dont_repeat_yourself/1, verify_max_module_length/1,
         verify_max_anonymous_function_arity/1, verify_max_function_arity/1,
         verify_max_function_length/1, verify_no_debug_call/1, verify_no_common_caveats_call/1,
         verify_no_call/1, verify_no_nested_try_catch/1, verify_no_successive_maps/1,
         verify_atom_naming_convention/1, verify_no_throw/1, verify_no_dollar_space/1,
         verify_no_author/1, verify_no_import/1, verify_no_catch_expressions/1,
         verify_no_single_clause_case/1, verify_numeric_format/1, verify_behaviour_spelling/1,
         verify_always_shortcircuit/1, verify_consistent_generic_type/1, verify_no_types/1,
         verify_no_specs/1, verify_export_used_types/1, verify_consistent_variable_casing/1,
         verify_no_match_in_condition/1, verify_param_pattern_matching/1,
         verify_private_data_types/1, verify_unquoted_atoms/1]).
%% -elvis attribute
-export([verify_elvis_attr_atom_naming_convention/1, verify_elvis_attr_numeric_format/1,
         verify_elvis_attr_dont_repeat_yourself/1, verify_elvis_attr_function_naming_convention/1,
         verify_elvis_attr_god_modules/1, verify_elvis_attr_invalid_dynamic_call/1,
         verify_elvis_attr_line_length/1, verify_elvis_attr_macro_module_names/1,
         verify_elvis_attr_macro_names/1, verify_elvis_attr_max_anonymous_function_arity/1,
         verify_elvis_attr_max_function_arity/1, verify_elvis_attr_max_function_length/1,
         verify_elvis_attr_max_module_length/1, verify_elvis_attr_module_naming_convention/1,
         verify_elvis_attr_nesting_level/1, verify_elvis_attr_no_behavior_info/1,
         verify_elvis_attr_no_call/1, verify_elvis_attr_no_debug_call/1,
         verify_elvis_attr_no_if_expression/1, verify_elvis_attr_no_nested_try_catch/1,
         verify_elvis_attr_no_successive_maps/1, verify_elvis_attr_no_spec_with_records/1,
         verify_elvis_attr_no_tabs/1, verify_elvis_attr_no_trailing_whitespace/1,
         verify_elvis_attr_operator_spaces/1, verify_elvis_attr_state_record_and_type/1,
         verify_elvis_attr_used_ignored_variable/1, verify_elvis_attr_variable_naming_convention/1,
         verify_elvis_attr_behaviour_spelling/1, verify_elvis_attr_param_pattern_matching/1,
         verify_elvis_attr_private_data_types/1]).
%% Non-rule
-export([results_are_ordered_by_line/1, oddities/1]).

-define(EXCLUDED_FUNS,
        [module_info,
         all,
         groups,
         test,
         init_per_suite,
         end_per_suite,
         init_per_group,
         end_per_group]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)] ++ [{group, beam_files}].

-spec groups() -> [{beam_files, [sequence], [atom()]}].
groups() ->
    [{beam_files,
      [sequence],
      [verify_function_naming_convention, verify_variable_naming_convention,
       verify_consistent_variable_casing, verify_nesting_level, verify_god_modules,
       verify_no_if_expression, verify_invalid_dynamic_call, verify_used_ignored_variable,
       verify_no_behavior_info, verify_module_naming_convention, verify_state_record_and_type,
       verify_state_record_and_type_plus_export_used_types, verify_no_spec_with_records,
       verify_dont_repeat_yourself, verify_no_debug_call, verify_no_common_caveats_call,
       verify_no_call, verify_no_nested_try_catch, verify_no_successive_maps,
       verify_atom_naming_convention, verify_no_throw, verify_no_author, verify_no_import,
       verify_always_shortcircuit, verify_no_catch_expressions, verify_no_single_clause_case,
       verify_no_macros, verify_export_used_types, verify_max_anonymous_function_arity,
       verify_max_function_arity, verify_no_match_in_condition, verify_behaviour_spelling,
       verify_param_pattern_matching, verify_private_data_types, verify_unquoted_atoms]}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    _ = application:ensure_all_started(elvis_core),
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

-spec init_per_group(atom(), config()) -> config().
init_per_group(beam_files = Group, Config) ->
    [{test_file_ext, "beam"}, {group, Group} | Config];
init_per_group(_Group, Config) ->
    Config.

-spec end_per_group(atom(), config()) -> config().
end_per_group(_Group, Config) ->
    proplists:delete(test_file_ext, proplists:delete(group, Config)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Rules

-spec verify_function_naming_convention(config()) -> any().
verify_function_naming_convention(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % pass
    PathPass = "pass_function_naming_convention." ++ Ext,

    RuleConfig = #{regex => "^([a-z][a-z0-9]*_?)*$"},
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              function_naming_convention,
                              RuleConfig,
                              PathPass),

    RuleConfig2 =
        #{regex => "^([a-z][a-z0-9]*_?)*$", ignore => [fail_function_naming_convention]},
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              function_naming_convention,
                              RuleConfig2,
                              PathPass),

    % fail
    PathFail = "fail_function_naming_convention." ++ Ext,
    [_CamelCaseError,
     _ALL_CAPSError,
     _InitialCapError,
     _HyphenError,
     _PredError,
     _EmailError] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              function_naming_convention,
                              RuleConfig,
                              PathFail),

    RuleConfig3 =
        #{regex => "^([a-z][a-z0-9]*_?)*$",
          ignore =>
              [{fail_function_naming_convention, camelCase},
               {fail_function_naming_convention, 'ALL_CAPS'},
               {fail_function_naming_convention, 'Initial_cap'},
               {fail_function_naming_convention, 'ok-for-lisp'},
               {fail_function_naming_convention, 'no_predicates?'}]},
    [_EmailError2] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              function_naming_convention,
                              RuleConfig3,
                              PathFail),

    % ignored
    PathIgnored = "fail_function_naming_convention_ignored_function." ++ Ext,

    RuleConfig4 =
        #{regex => "^([a-z][a-z0-9]*_?)*$",
          ignore =>
              [{fail_function_naming_convention, camelCase},
               {fail_function_naming_convention, 'ALL_CAPS'},
               {fail_function_naming_convention, 'Initial_cap'},
               {fail_function_naming_convention, 'ok-for-lisp'},
               {fail_function_naming_convention, 'no_predicates?'},
               {fail_function_naming_convention, user@location}]},
    [_AnError] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              function_naming_convention,
                              RuleConfig4,
                              PathIgnored).

-spec verify_variable_naming_convention(config()) -> any().
verify_variable_naming_convention(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    RuleConfig = #{regex => "^_?([A-Z][0-9a-zA-Z]*)$"},

    PathPass = "pass_variable_naming_convention." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              variable_naming_convention,
                              RuleConfig,
                              PathPass),

    PathFail = "fail_variable_naming_convention." ++ Ext,
    [_AtSign,
     _Underline_Word_Separator,
     _Bad_Ignored_Variable,
     _AtSignAgain,
     _Underline_Word_SeparatorAgain] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              variable_naming_convention,
                              RuleConfig,
                              PathFail).

-spec verify_consistent_variable_casing(config()) -> any().
verify_consistent_variable_casing(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),
    PathPass = "pass_consistent_variable_casing." ++ Ext,
    [] =
        elvis_core_apply_rule(Config, elvis_style, consistent_variable_casing, #{}, PathPass),

    PathFail = "fail_consistent_variable_casing." ++ Ext,
    [#{info := ["TypeVar", _, ["Typevar"]]},
     #{info :=
           ["GeneralInconsistency",
            _,
            ["GENERALInconsistencY",
             "GENERALInconsistency",
             "GeNeRaLiNcOnSiStEnCy",
             "GeneralINCONSISTENCY"]]},
     #{info := ["SpecVar", _, ["SPECVar"]]},
     #{info := ["FuncVar", _, ["FUNCVar"]]},
     #{info := ["FunVar", _, ["FunVAR"]]},
     #{info := ["IgnVar", _, ["IGNVar"]]}] =
        elvis_core_apply_rule(Config, elvis_style, consistent_variable_casing, #{}, PathFail).

-spec verify_line_length_rule(config()) -> any().
verify_line_length_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_line_length." ++ Ext,

    Result =
        elvis_core_apply_rule(Config, elvis_text_style, line_length, #{limit => 100}, Path),
    8 = length(Result),
    #{info := Info, message := Msg} = lists:nth(7, Result),
    <<"Line 32 is too long. It has ", _/binary>> = list_to_binary(io_lib:format(Msg, Info)),

    WholeLineResult =
        elvis_core_apply_rule(Config,
                              elvis_text_style,
                              line_length,
                              #{limit => 100, skip_comments => whole_line},
                              Path),
    6 = length(WholeLineResult),

    AnyResult =
        elvis_core_apply_rule(Config,
                              elvis_text_style,
                              line_length,
                              #{limit => 100, skip_comments => any},
                              Path),
    6 = length(AnyResult).

-spec verify_line_length_rule_latin1(config()) -> any().
verify_line_length_rule_latin1(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_line_length_latin1." ++ Ext,

    Result =
        elvis_core_apply_rule(Config, elvis_text_style, line_length, #{limit => 100}, Path),
    1 = length(Result),
    #{info := Info, message := Msg} = lists:nth(1, Result),
    <<"Line 13 is too long. It has", _/binary>> = list_to_binary(io_lib:format(Msg, Info)).

-spec verify_unicode_line_length_rule(config()) -> any().
verify_unicode_line_length_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "pass_unicode_comments." ++ Ext,

    Result =
        elvis_core_apply_rule(Config, elvis_text_style, line_length, #{limit => 100}, Path),
    0 = length(Result).

-spec verify_no_tabs_rule(config()) -> any().
verify_no_tabs_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_tabs." ++ Ext,

    [_, _] = elvis_core_apply_rule(Config, elvis_text_style, no_tabs, #{}, Path).

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
        elvis_core_apply_rule(Config, elvis_text_style, no_trailing_whitespace, RuleConfig, Path),
    length(Items) == ExpectedNumItems
    orelse ct:fail("Expected ~b error items. Got: ~p", [ExpectedNumItems, Items]).

-spec verify_macro_names_rule(config()) -> any().
verify_macro_names_rule(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_macro_names." ++ Ext,

    [_, _, _, _, _, _] = elvis_core_apply_rule(Config, elvis_style, macro_names, #{}, Path),

    [_, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              macro_names,
                              #{regex => "^[A-Za-z_ ]+$"},
                              Path),

    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              macro_names,
                              #{regex => "^[A-Za-z_ \-]+$"},
                              Path),

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              macro_names,
                              #{regex => "^[A-Za-z_, \-]+$"},
                              Path),

    [_, _, _, _, _, _, _, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              macro_names,
                              #{regex => "^POTENTIAL_BAD-NAME$"},
                              Path),

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              macro_names,
                              #{ignore => [fail_macro_names]},
                              Path).

-spec verify_macro_module_names(config()) -> any().
verify_macro_module_names(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_macro_module_names." ++ Ext,
    [#{line_num := 20},
     #{line_num := 20},
     #{line_num := 21},
     #{line_num := 22},
     #{line_num := 23},
     #{line_num := 23},
     #{line_num := 27},
     #{line_num := 28},
     #{line_num := 29}] =
        elvis_core_apply_rule(Config, elvis_style, macro_module_names, #{}, Path).

-spec verify_no_macros(config()) -> any().
verify_no_macros(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_no_macros." ++ Ext,
    FailRes = elvis_core_apply_rule(Config, elvis_style, no_macros, #{}, PathFail),
    case Ext of
        "beam" ->
            [] = FailRes; % no macros on BEAM files
        _ ->
            [_] = FailRes
    end,

    PathPass = "pass_no_macros." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              no_macros,
                              #{allow => ['ALLOWED_MACRO']},
                              PathPass).

-spec verify_no_types(config()) -> any().
verify_no_types(Config) ->
    PathFail = "fail_no_types.hrl",
    [#{line_num := 1}] = elvis_core_apply_rule(Config, elvis_style, no_types, #{}, PathFail),

    PathPass = "pass_no_types.hrl",
    [] = elvis_core_apply_rule(Config, elvis_style, no_types, #{}, PathPass).

-spec verify_no_specs(config()) -> any().
verify_no_specs(Config) ->
    PathFail = "fail_no_specs.hrl",
    [#{line_num := 3}] = elvis_core_apply_rule(Config, elvis_style, no_specs, #{}, PathFail),

    PathPass = "pass_no_specs.hrl",
    [] = elvis_core_apply_rule(Config, elvis_style, no_specs, #{}, PathPass).

-spec verify_no_block_expressions(config()) -> any().
verify_no_block_expressions(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_block_expressions." ++ Ext,

    [#{info := [9]}] =
        elvis_core_apply_rule(Config, elvis_style, no_block_expressions, #{}, Path).

-spec verify_operator_spaces(config()) -> any().
verify_operator_spaces(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_operator_spaces." ++ Ext,

    [] = elvis_core_apply_rule(Config, elvis_style, operator_spaces, #{rules => []}, Path),

    RuleConfig = #{rules => [{right, ","}]},
    [_, _, _] = elvis_core_apply_rule(Config, elvis_style, operator_spaces, RuleConfig, Path),

    AppendOptions = #{rules => [{right, "++"}, {left, "++"}]},
    [_] = elvis_core_apply_rule(Config, elvis_style, operator_spaces, AppendOptions, Path),

    SumOperation = #{rules => [{right, "+"}, {left, "+"}]},
    [_, _, _, _] =
        elvis_core_apply_rule(Config, elvis_style, operator_spaces, SumOperation, Path),

    MinusOperation = #{rules => [{right, "-"}, {left, "-"}]},
    [_, _] =
        elvis_core_apply_rule(Config, elvis_style, operator_spaces, MinusOperation, Path),

    Arrow = #{rules => [{left, "->"}]},
    [_, _] = elvis_core_apply_rule(Config, elvis_style, operator_spaces, Arrow, Path),

    BarOptions = #{rules => [{right, "|"}, {left, "|"}]},
    [_, _, _, _] =
        elvis_core_apply_rule(Config, elvis_style, operator_spaces, BarOptions, Path),

    ComprehensionOperation = #{rules => [{right, "||"}, {left, "||"}]},
    [_, _, _, _, _, _] =
        elvis_core_apply_rule(Config, elvis_style, operator_spaces, ComprehensionOperation, Path),

    DefaultOptions = #{},
    [#{info := [right, "," | _]}, #{info := [right, "," | _]}, #{info := [left, "++" | _]},
     #{info := [right, "," | _]}, #{info := [left, "+" | _]}, #{info := [right, "+" | _]},
     #{info := [right, "|" | _]}, #{info := [left, "|" | _]}, #{info := [right, "||" | _]},
     #{info := [left, "||" | _]}, #{info := [right, "::" | _]}, #{info := [left, "::" | _]},
     #{info := [right, "->" | _]}, #{info := [left, "->" | _]}, #{info := [left, "->" | _]},
     #{info := [right, "+" | _]}, #{info := [left, "+" | _]}, #{info := [right, "-" | _]},
     #{info := [left, "-" | _]}, #{info := [right, "*" | _]}, #{info := [left, "*" | _]},
     #{info := [right, "/" | _]}, #{info := [left, "/" | _]}, #{info := [right, "=<" | _]},
     #{info := [left, "=<" | _]}, #{info := [right, "<" | _]}, #{info := [left, "<" | _]},
     #{info := [right, ">" | _]}, #{info := [left, ">" | _]}, #{info := [right, ">=" | _]},
     #{info := [left, ">=" | _]}, #{info := [right, "==" | _]}, #{info := [left, "==" | _]},
     #{info := [right, "=:=" | _]}, #{info := [left, "=:=" | _]}, #{info := [right, "/=" | _]},
     #{info := [left, "/=" | _]}, #{info := [right, "=/=" | _]}, #{info := [left, "=/=" | _]},
     #{info := [right, "--" | _]}, #{info := [left, "--" | _]}, #{info := [right, "||" | _]},
     #{info := [left, "||" | _]}, #{info := [right, "||" | _]}, #{info := [left, "||" | _]},
     #{info := [right, "|" | _]}, #{info := [left, "|" | _]}, #{info := [left, "!" | _]},
     #{info := [right, "!" | _]}] =
        elvis_core_apply_rule(Config, elvis_style, operator_spaces, DefaultOptions, Path).

-spec verify_no_space(config()) -> any().
verify_no_space(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path1 = "fail_no_space." ++ Ext,
    [#{info := [right, "(", 3]},
     #{info := [right, "(", 36]},
     #{info := [right, "(", 52]},
     #{info := [left, ")", 52]},
     #{info := [left, ",", 76]},
     #{info := [left, ")", 79]},
     #{info := [right, "(", 109]},
     #{info := [left, ")", 109]},
     #{info := [right, "#", 121]},
     #{info := [right, "?", 121]}] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              no_space,
                              elvis_style:default(no_space),
                              Path1).

-spec verify_no_space_after_pound(config()) -> any().
verify_no_space_after_pound(Config) ->
    PathFail = "fail_no_space_after_pound.erl",
    [#{line_num := 5},
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
     #{line_num := 22}] =
        elvis_core_apply_rule(Config, elvis_style, no_space_after_pound, #{}, PathFail),

    PathPass = "pass_no_space_after_pound.erl",
    [] = elvis_core_apply_rule(Config, elvis_style, no_space_after_pound, #{}, PathPass),
    ok.

-spec verify_operator_spaces_latin1(config()) -> any().
verify_operator_spaces_latin1(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_operator_spaces_latin1." ++ Ext,

    [] = elvis_core_apply_rule(Config, elvis_style, operator_spaces, #{rules => []}, Path),

    AppendOptions = #{rules => [{right, "++"}, {left, "++"}]},
    [_, _] = elvis_core_apply_rule(Config, elvis_style, operator_spaces, AppendOptions, Path).

-spec verify_nesting_level(config()) -> any().
verify_nesting_level(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_nesting_level." ++ Ext,

    _ = case Group of
            beam_files ->
                [#{line_num := 9},
                 #{line_num := 12},
                 #{line_num := 23},
                 #{line_num := 39},
                 #{line_num := 69},
                 #{line_num := 108},
                 #{line_num := 153},
                 #{line_num := 170}] =
                    elvis_core_apply_rule(Config, elvis_style, nesting_level, #{level => 3}, Path);
            erl_files ->
                [#{line_num := 11},
                 #{line_num := 18},
                 #{line_num := 30},
                 #{line_num := 45},
                 #{line_num := 78},
                 #{line_num := 120},
                 #{line_num := 166},
                 #{line_num := 182}] =
                    elvis_core_apply_rule(Config, elvis_style, nesting_level, #{level => 3}, Path)
        end,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              nesting_level,
                              #{ignore => [fail_nesting_level]},
                              Path).

-spec verify_god_modules(config()) -> any().
verify_god_modules(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_god_modules." ++ Ext,
    [_] = elvis_core_apply_rule(Config, elvis_style, god_modules, #{limit => 25}, Path),

    RuleConfig = #{limit => 25, ignore => [fail_god_modules]},
    [] = elvis_core_apply_rule(Config, elvis_style, god_modules, RuleConfig, Path).

-spec verify_no_if_expression(config()) -> any().
verify_no_if_expression(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_if_expression." ++ Ext,
    _ = case Group of
            beam_files ->
                [#{line_num := 8}, #{line_num := 18}, #{line_num := 26}] =
                    elvis_core_apply_rule(Config, elvis_style, no_if_expression, #{}, Path);
            erl_files ->
                [#{line_num := 11}, #{line_num := 22}, #{line_num := 31}] =
                    elvis_core_apply_rule(Config, elvis_style, no_if_expression, #{}, Path)
        end.

-spec verify_invalid_dynamic_call(config()) -> any().
verify_invalid_dynamic_call(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_invalid_dynamic_call." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, invalid_dynamic_call, #{}, PathPass),

    PathFail = "fail_invalid_dynamic_call." ++ Ext,
    _ = case Group of
            beam_files ->
                [#{line_num := _},
                 #{line_num := _},
                 #{line_num := _},
                 #{line_num := _},
                 #{line_num := _},
                 #{line_num := _},
                 #{line_num := _}] =
                    elvis_core_apply_rule(Config, elvis_style, invalid_dynamic_call, #{}, PathFail);
            erl_files ->
                [#{line_num := 19},
                 #{line_num := 31},
                 #{line_num := 32},
                 #{line_num := 40},
                 #{line_num := 48},
                 #{line_num := 59},
                 #{line_num := 66}] =
                    elvis_core_apply_rule(Config, elvis_style, invalid_dynamic_call, #{}, PathFail)
        end,

    RuleConfig = #{ignore => [fail_invalid_dynamic_call]},
    [] =
        elvis_core_apply_rule(Config, elvis_style, invalid_dynamic_call, RuleConfig, PathFail).

-spec verify_used_ignored_variable(config()) -> any().
verify_used_ignored_variable(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_used_ignored_variable." ++ Ext,
    _ = case Group of
            beam_files ->
                [#{line_num := _}, #{line_num := _}, #{line_num := _}, #{line_num := _}] =
                    elvis_core_apply_rule(Config, elvis_style, used_ignored_variable, #{}, Path);
            erl_files ->
                [#{line_num := 31}, #{line_num := 34}, #{line_num := 38}, #{line_num := 38}] =
                    elvis_core_apply_rule(Config, elvis_style, used_ignored_variable, #{}, Path)
        end,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              used_ignored_variable,
                              #{ignore => [fail_used_ignored_variable]},
                              Path).

-spec verify_no_behavior_info(config()) -> any().
verify_no_behavior_info(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Path = "fail_no_behavior_info." ++ Ext,
    _ = case Group of
            beam_files ->
                [#{line_num := 7}, #{line_num := 10}] =
                    elvis_core_apply_rule(Config, elvis_style, no_behavior_info, #{}, Path);
            erl_files ->
                [#{line_num := 14}, #{line_num := 17}] =
                    elvis_core_apply_rule(Config, elvis_style, no_behavior_info, #{}, Path)
        end.

-spec verify_module_naming_convention(config()) -> any().
verify_module_naming_convention(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    RuleConfig = #{regex => "^([a-z][a-z0-9]*_?)*$", ignore => []},

    PathPass = "pass_module_naming_convention." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              module_naming_convention,
                              RuleConfig,
                              PathPass),

    PathFail = "fail_module_naming_1_convention_1." ++ Ext,
    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              module_naming_convention,
                              RuleConfig,
                              PathFail),

    RuleConfigIgnore = RuleConfig#{ignore => [fail_module_naming_1_convention_1]},
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              module_naming_convention,
                              RuleConfigIgnore,
                              PathFail).

-spec verify_state_record_and_type(config()) -> any().
verify_state_record_and_type(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_state_record_and_type." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, state_record_and_type, #{}, PathPass),

    PathPassWithOpaque = "pass_state_record_and_type_opaque." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              state_record_and_type,
                              #{},
                              PathPassWithOpaque),

    PathPassGenStateM = "pass_state_record_and_type_gen_statem." ++ Ext,
    [] =
        elvis_core_apply_rule(Config, elvis_style, state_record_and_type, #{}, PathPassGenStateM),

    PathFail = "fail_state_record_and_type." ++ Ext,
    [_] = elvis_core_apply_rule(Config, elvis_style, state_record_and_type, #{}, PathFail),

    PathFail1 = "fail_state_type." ++ Ext,
    [_] = elvis_core_apply_rule(Config, elvis_style, state_record_and_type, #{}, PathFail1),

    PathBehaviourFail = "fail_state_record_and_type_behaviour." ++ Ext,
    [_] =
        elvis_core_apply_rule(Config, elvis_style, state_record_and_type, #{}, PathBehaviourFail),

    PathFailGenStateMType = "fail_state_record_and_type_gen_statem_type." ++ Ext,
    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              state_record_and_type,
                              #{},
                              PathFailGenStateMType),

    PathPassGenStateMState = "fail_state_record_and_type_gen_statem_state." ++ Ext,
    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              state_record_and_type,
                              #{},
                              PathPassGenStateMState).

-spec verify_state_record_and_type_plus_export_used_types(config()) -> any().
verify_state_record_and_type_plus_export_used_types(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_state_record_and_type_plus_export_used_types." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, state_record_and_type, #{}, PathPass),
    [] = elvis_core_apply_rule(Config, elvis_style, export_used_types, #{}, PathPass),

    PathPassGenStateM =
        "pass_state_record_and_type_plus_export_used_types_gen_statem." ++ Ext,
    [] =
        elvis_core_apply_rule(Config, elvis_style, state_record_and_type, #{}, PathPassGenStateM),
    [] =
        elvis_core_apply_rule(Config, elvis_style, export_used_types, #{}, PathPassGenStateM),

    PathFail = "fail_state_record_and_type_plus_export_used_types." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, state_record_and_type, #{}, PathFail),
    [_] = elvis_core_apply_rule(Config, elvis_style, export_used_types, #{}, PathFail).

-spec verify_behaviour_spelling(config()) -> any().
verify_behaviour_spelling(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "british_behaviour_spelling." ++ Ext,
    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              behaviour_spelling,
                              #{spelling => behavior},
                              PathFail),
    PathFail1 = "american_behavior_spelling." ++ Ext,
    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              behaviour_spelling,
                              #{spelling => behaviour},
                              PathFail1),

    PathPass = "british_behaviour_spelling." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              behaviour_spelling,
                              #{spelling => behaviour},
                              PathPass),
    PathPass1 = "american_behavior_spelling." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              behaviour_spelling,
                              #{spelling => behavior},
                              PathPass1).

-spec verify_param_pattern_matching(config()) -> any().
verify_param_pattern_matching(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathRight = "right_param_pattern_matching." ++ Ext,
    PathLeft = "left_param_pattern_matching." ++ Ext,
    [#{info := ['Simple' | _]},
     #{info := ['SimpleToo' | _]},
     #{info := ['The' | _]},
     #{info := ['TheToo' | _]},
     #{info := ['AsYoda' | _]},
     #{info := ['AsYodaToo' | _]}] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              param_pattern_matching,
                              #{side => left},
                              PathRight),
    [#{info := ['Simple' | _]},
     #{info := ['SimpleToo' | _]},
     #{info := ['Multiple' | _]},
     #{info := ['MultipleToo' | _]},
     #{info := ['TheSecond' | _]},
     #{info := ['TheSecondToo' | _]},
     #{info := ['But' | _]},
     #{info := ['ButToo' | _]}] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              param_pattern_matching,
                              #{side => right},
                              PathLeft),

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              param_pattern_matching,
                              #{side => right},
                              PathRight),

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              param_pattern_matching,
                              #{side => left},
                              PathLeft).

-spec verify_consistent_generic_type(config()) -> any().
verify_consistent_generic_type(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "consistent_generic_type_term." ++ Ext,
    [_, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              consistent_generic_type,
                              #{preferred_type => any},
                              PathFail),
    PathFail1 = "consistent_generic_type_any." ++ Ext,
    [_, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              consistent_generic_type,
                              #{preferred_type => term},
                              PathFail1),
    PathFail2 = "consistent_generic_type_term_and_any." ++ Ext,
    [_, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              consistent_generic_type,
                              #{preferred_type => any},
                              PathFail2),
    PathFail3 = "consistent_generic_type_term_and_any." ++ Ext,
    [_, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              consistent_generic_type,
                              #{preferred_type => term},
                              PathFail3),

    PathPass = "consistent_generic_type_term." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              consistent_generic_type,
                              #{preferred_type => term},
                              PathPass),
    PathPass1 = "consistent_generic_type_any." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              consistent_generic_type,
                              #{preferred_type => any},
                              PathPass1),
    PathPass2 = "consistent_generic_type_no_checks." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              consistent_generic_type,
                              #{preferred_type => term},
                              PathPass2),
    PathPass2 = "consistent_generic_type_no_checks." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              consistent_generic_type,
                              #{preferred_type => any},
                              PathPass2).

-spec verify_always_shortcircuit(config()) -> any().
verify_always_shortcircuit(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_always_shortcircuit." ++ Ext,
    [_, _, _, _] =
        elvis_core_apply_rule(Config, elvis_style, always_shortcircuit, #{}, PathFail),

    PathPass = "pass_always_shortcircuit." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, always_shortcircuit, #{}, PathPass).

-spec verify_no_spec_with_records(config()) -> any().
verify_no_spec_with_records(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_no_spec_with_records." ++ Ext,
    [_, _, _] =
        elvis_core_apply_rule(Config, elvis_style, no_spec_with_records, #{}, PathFail),

    PathPass = "pass_no_spec_with_records." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, no_spec_with_records, #{}, PathPass).

-spec verify_dont_repeat_yourself(config()) -> any().
verify_dont_repeat_yourself(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_dont_repeat_yourself." ++ Ext,
    RuleConfig5 = #{min_complexity => 5},
    Res1 =
        elvis_core_apply_rule(Config, elvis_style, dont_repeat_yourself, RuleConfig5, PathFail),
    2 = length(Res1),

    RuleConfig9 = #{min_complexity => 9},
    Res2 =
        elvis_core_apply_rule(Config, elvis_style, dont_repeat_yourself, RuleConfig9, PathFail),
    1 = length(Res2),

    IgnoreRule = #{ignore => [fail_dont_repeat_yourself]},
    [] =
        elvis_core_apply_rule(Config, elvis_style, dont_repeat_yourself, IgnoreRule, PathFail),

    PathPass = "pass_dont_repeat_yourself." ++ Ext,
    [] =
        elvis_core_apply_rule(Config, elvis_style, dont_repeat_yourself, RuleConfig5, PathPass).

-spec verify_max_module_length(config()) -> any().
verify_max_module_length(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_max_module_length." ++ Ext,

    CountAllRuleConfig = #{count_comments => true, count_whitespace => true},

    ct:comment("Count whitespace and comment lines"),
    RuleConfig = CountAllRuleConfig#{max_length => 10},

    [_] = elvis_core_apply_rule(Config, elvis_style, max_module_length, RuleConfig, PathFail),

    RuleConfig1 = CountAllRuleConfig#{max_length => 14},
    [_] =
        elvis_core_apply_rule(Config, elvis_style, max_module_length, RuleConfig1, PathFail),

    RuleConfig2 = CountAllRuleConfig#{max_length => 15},
    [] = elvis_core_apply_rule(Config, elvis_style, max_module_length, RuleConfig2, PathFail),

    ct:comment("Don't count whitespace lines"),
    WhitespaceRuleConfig = CountAllRuleConfig#{count_whitespace => false},

    RuleConfig3 = WhitespaceRuleConfig#{max_length => 3},
    [_] =
        elvis_core_apply_rule(Config, elvis_style, max_module_length, RuleConfig3, PathFail),

    RuleConfig4 = WhitespaceRuleConfig#{max_length => 4},
    [_] =
        elvis_core_apply_rule(Config, elvis_style, max_module_length, RuleConfig4, PathFail),

    RuleConfig5 = WhitespaceRuleConfig#{max_length => 5},
    [] = elvis_core_apply_rule(Config, elvis_style, max_module_length, RuleConfig5, PathFail),

    ct:comment("Don't count comment or whitespace lines"),
    NoCountRuleConfig = WhitespaceRuleConfig#{count_comments => false},

    RuleConfig6 = NoCountRuleConfig#{max_length => 1},
    [_] =
        elvis_core_apply_rule(Config, elvis_style, max_module_length, RuleConfig6, PathFail),

    RuleConfig7 = NoCountRuleConfig#{max_length => 2},
    [_] =
        elvis_core_apply_rule(Config, elvis_style, max_module_length, RuleConfig7, PathFail),

    RuleConfig8 = NoCountRuleConfig#{max_length => 3},
    [] = elvis_core_apply_rule(Config, elvis_style, max_module_length, RuleConfig8, PathFail),

    {comment, ""}.

-spec verify_max_function_arity(config()) -> any().
verify_max_function_arity(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_max_function_arity." ++ Ext,
    RuleConfig = #{max_arity => 8},

    [] = elvis_core_apply_rule(Config, elvis_style, max_function_arity, RuleConfig, PathPass),

    %% This module has functions with 0, 1, 2, and 3 arguments
    PathFail = "fail_max_function_arity." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, max_function_arity, #{}, PathFail),
    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              max_function_arity,
                              #{max_arity => 2},
                              PathFail),
    [_, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              max_function_arity,
                              #{max_arity => 1},
                              PathFail),
    [_, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              max_function_arity,
                              #{max_arity => 0},
                              PathFail),
    [_, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              max_function_arity,
                              #{max_arity => -1},
                              PathFail),

    ok.

-spec verify_max_anonymous_function_arity(config()) -> any().
verify_max_anonymous_function_arity(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathPass = "pass_max_anonymous_function_arity." ++ Ext,
    RuleConfig = #{max_arity => 3},

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              max_anonymous_function_arity,
                              RuleConfig,
                              PathPass),

    %% This module has funs with 0, 1, 2, and 3 arguments
    PathFail = "fail_max_anonymous_function_arity." ++ Ext,
    [] =
        elvis_core_apply_rule(Config, elvis_style, max_anonymous_function_arity, #{}, PathFail),
    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              max_anonymous_function_arity,
                              #{max_arity => 2},
                              PathFail),
    [_, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              max_anonymous_function_arity,
                              #{max_arity => 1},
                              PathFail),
    [_, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              max_anonymous_function_arity,
                              #{max_arity => 0},
                              PathFail),
    [_, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              max_anonymous_function_arity,
                              #{max_arity => -1},
                              PathFail),

    ok.

-spec verify_max_function_length(config()) -> any().
verify_max_function_length(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_max_function_length." ++ Ext,
    ModuleFail = fail_max_function_length,

    CountAllRuleConfig = #{count_comments => true, count_whitespace => true},

    ct:comment("Count whitespace and comment lines"),
    RuleConfig = CountAllRuleConfig#{max_length => 4},
    [_, _, _] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig, PathFail),

    RuleConfig1 = CountAllRuleConfig#{max_length => 9},
    [_, _] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig1, PathFail),

    RuleConfig2 = CountAllRuleConfig#{max_length => 14},
    [_] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig2, PathFail),

    RuleConfig3 = CountAllRuleConfig#{max_length => 15},
    [] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig3, PathFail),

    ct:comment("Don't count whitespace lines"),
    WhitespaceRuleConfig = CountAllRuleConfig#{count_whitespace => false},

    RuleConfig4 = WhitespaceRuleConfig#{max_length => 3},
    [_, _, _] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig4, PathFail),

    RuleConfig5 = WhitespaceRuleConfig#{max_length => 7},
    [_, _] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig5, PathFail),

    RuleConfig6 = WhitespaceRuleConfig#{max_length => 8},
    [_] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig6, PathFail),

    RuleConfig7 = WhitespaceRuleConfig#{max_length => 11},
    [_] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig7, PathFail),

    RuleConfig8 = WhitespaceRuleConfig#{max_length => 12},
    [] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig8, PathFail),

    ct:comment("Don't count comment or whitespace lines"),
    NoCountRuleConfig = WhitespaceRuleConfig#{count_comments => false},

    RuleConfig9 = NoCountRuleConfig#{max_length => 1},
    [_, _, _] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig9, PathFail),

    RuleConfig10 = NoCountRuleConfig#{max_length => 2},
    [] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig10, PathFail),

    IgnoredFunctions = [{ModuleFail, f15}, {ModuleFail, f10, 1}],
    RuleConfig11 = RuleConfig5#{ignore => IgnoredFunctions},
    [] =
        elvis_core_apply_rule(Config, elvis_style, max_function_length, RuleConfig11, PathFail),

    {comment, ""}.

-spec verify_no_debug_call(config()) -> any().
verify_no_debug_call(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_no_debug_call." ++ Ext,

    PathFail = "fail_no_debug_call." ++ Ext,

    _ = case Group of
            beam_files -> % io:format is preprocessed
                [_, _, _, _, _] =
                    elvis_core_apply_rule(Config, elvis_style, no_debug_call, #{}, PathFail);
            erl_files ->
                [_, _, _, _, _, _, _] =
                    elvis_core_apply_rule(Config, elvis_style, no_debug_call, #{}, PathFail)
        end,

    RuleConfig = #{ignore => [fail_no_debug_call]},
    [] = elvis_core_apply_rule(Config, elvis_style, no_debug_call, RuleConfig, PathFail),

    RuleConfig2 = #{debug_functions => [{ct, pal, 2}]},
    _ = case Group of
            beam_files -> % ct:pal is preprocessed
                [] =
                    elvis_core_apply_rule(Config,
                                          elvis_style,
                                          no_debug_call,
                                          RuleConfig2,
                                          PathFail);
            erl_files ->
                [_] =
                    elvis_core_apply_rule(Config, elvis_style, no_debug_call, RuleConfig2, PathFail)
        end,

    RuleConfig3 = #{debug_functions => [{ct, pal}]},
    _ = case Group of
            beam_files -> % ct:pal is preprocessed
                [] =
                    elvis_core_apply_rule(Config,
                                          elvis_style,
                                          no_debug_call,
                                          RuleConfig3,
                                          PathFail);
            erl_files ->
                [_, _] =
                    elvis_core_apply_rule(Config, elvis_style, no_debug_call, RuleConfig3, PathFail)
        end,

    RuleConfig4 = #{debug_functions => [{io, format}]},
    [_, _, _] =
        elvis_core_apply_rule(Config, elvis_style, no_debug_call, RuleConfig4, PathFail),

    RuleConfig5 = #{debug_functions => [{ct, print}]},
    [_, _] = elvis_core_apply_rule(Config, elvis_style, no_debug_call, RuleConfig5, PathFail).

%% We test no_call and no_common_caveats_call by building the equivalent config and make sure that
%% other than defaults, they behave the same
-spec verify_no_common_caveats_call(config()) -> any().
verify_no_common_caveats_call(Config) ->
    verify_no_call_flavours(Config, no_common_caveats_call, caveat_functions, 6).

-spec verify_no_call(config()) -> any().
verify_no_call(Config) ->
    verify_no_call_flavours(Config, no_call, no_call_functions, 0).

-spec verify_no_call_flavours(any(), atom(), atom(), non_neg_integer()) -> any().
verify_no_call_flavours(Config,
                        RuleName,
                        RuleConfigMapKey,
                        ExpectedDefaultRuleMatchCount) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PathFail = "fail_no_call_classes." ++ Ext,

    assert_length(ExpectedDefaultRuleMatchCount,
                  elvis_core_apply_rule(Config, elvis_style, RuleName, #{}, PathFail),
                  RuleName),

    RuleConfig = #{ignore => [fail_no_call_classes]},
    assert_length(0,
                  elvis_core_apply_rule(Config, elvis_style, RuleName, RuleConfig, PathFail),
                  RuleName),

    RuleMatchTuples =
        [{{timer, send_after, 2}, 1},
         {{timer, send_after, 3}, 1},
         {{timer, send_interval, 2}, 1},
         {{timer, send_interval, 3}, 1},
         {{erlang, size, 1}, 2},
         {{timer, send_after}, 2},
         {{timer, '_', '_'}, 4},
         {{'_', tuple_size, 1}, 1}],

    lists:foreach(fun({FunSpec, ExpectedCount}) ->
                     ThisRuleConfig = maps:from_list([{RuleConfigMapKey, [FunSpec]}]),
                     Result =
                         elvis_core_apply_rule(Config,
                                               elvis_style,
                                               RuleName,
                                               ThisRuleConfig,
                                               PathFail),
                     assert_length(ExpectedCount, Result, RuleName)
                  end,
                  RuleMatchTuples).

-spec verify_no_nested_try_catch(config()) -> any().
verify_no_nested_try_catch(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Module = fail_no_nested_try_catch,
    Path = atom_to_list(Module) ++ "." ++ Ext,
    _ = case Group of
            beam_files ->
                [#{line_num := 9}, #{line_num := 18}, #{line_num := 21}] =
                    elvis_core_apply_rule(Config, elvis_style, no_nested_try_catch, #{}, Path);
            erl_files ->
                [#{line_num := 15}, #{line_num := 30}, #{line_num := 37}] =
                    elvis_core_apply_rule(Config, elvis_style, no_nested_try_catch, #{}, Path)
        end,

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              no_nested_try_catch,
                              #{ignore => [Module]},
                              Path).

-spec verify_no_successive_maps(config()) -> any().
-if(?OTP_RELEASE < 27).

verify_no_successive_maps(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    Module = fail_no_successive_maps,
    Path = atom_to_list(Module) ++ "." ++ Ext,
    _ = case Group of
            beam_files ->
                [_, _, _] =
                    elvis_core_apply_rule(Config, elvis_style, no_successive_maps, #{}, Path);
            erl_files ->
                [#{line_num := 7}, #{line_num := 8}, #{line_num := 9}] =
                    elvis_core_apply_rule(Config, elvis_style, no_successive_maps, #{}, Path)
        end,

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              no_successive_maps,
                              #{ignore => [Module]},
                              Path).

-else.

verify_no_successive_maps(_Config) ->
    [].

-endif.

-spec verify_unquoted_atoms(config()) -> any().
verify_unquoted_atoms(Config) ->
    PassPath = "pass_unquoted_atoms." ++ "erl",
    [] =
        elvis_core_apply_rule(Config, elvis_text_style, prefer_unquoted_atoms, #{}, PassPath),

    FailPath = "fail_quoted_atoms." ++ "erl",
    [_, _] =
        elvis_core_apply_rule(Config, elvis_text_style, prefer_unquoted_atoms, #{}, FailPath).

-spec verify_atom_naming_convention(config()) -> any().
verify_atom_naming_convention(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    BaseRegex = "^[a-z](_?[a-z0-9]+)*(_SUITE)?$",

    % pass
    PassModule = pass_atom_naming_convention,
    PassPath = atom_to_list(PassModule) ++ "." ++ Ext,
    PassModule2 = pass_atom_naming_convention_exception_class,
    PassPath2 = atom_to_list(PassModule2) ++ "." ++ Ext,
    PassModule3 = pass_maybe,
    PassPath3 = atom_to_list(PassModule3) ++ "." ++ Ext,

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => BaseRegex},
                              PassPath),
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => "^[^xwyhr]*$"},
                              PassPath2),
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => "^^[a-z]([a-zA-Z0-9@]*_?)*(_SUITE)?$"},
                              PassPath3),

    % fail
    FailModule = fail_atom_naming_convention,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,
    FailModule2 = fail_atom_naming_convention_exception_class,
    FailPath2 = atom_to_list(FailModule2) ++ "." ++ Ext,

    [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => BaseRegex, enclosed_atoms => same},
                              FailPath),
    [_, _, _, _, _, _, _, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => "^([a-zA-Z_]+)$", enclosed_atoms => same},
                              FailPath),
    [_, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => "^([a-zA-Z_' \\\\]+)$", enclosed_atoms => same},
                              FailPath),
    [_, _, _, _, _, _, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => "^([a-zA-Z\-_]+)$", enclosed_atoms => same},
                              FailPath),
    [_, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => "^([a-zA-Z\-_' \\\\]+)$", enclosed_atoms => same},
                              FailPath),
    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => "^([0-9]?[a-zA-Z\-_]+)$"},
                              FailPath),
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => BaseRegex, ignore => [FailModule]},
                              FailPath),
    KeepRegex = "^([a-zA-Z0-9_]+)$",
    [_, _, _, _, _, _, _, _, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => KeepRegex, enclosed_atoms => "^([a-z][a-z0-9A-Z_]*)$"},
                              FailPath),
    [_, _, _, _, _, _, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => KeepRegex,
                                enclosed_atoms => "^([a-z][a-z0-9A-Z_' \\\\]*)$"},
                              FailPath),
    [_, _, _, _, _, _, _, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => KeepRegex, enclosed_atoms => "^([a-z][\-a-z0-9A-Z_]*)$"},
                              FailPath),
    [_, _, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => KeepRegex,
                                enclosed_atoms => "^([0-9a-z][\-a-z0-9A-Z_' \\\\]*)$"},
                              FailPath),
    [_] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              atom_naming_convention,
                              #{regex => "^[^xwyhr]*$"},
                              FailPath2),
    _ = case Group of
            beam_files -> % 'or_THIS' getting stripped of enclosing '
                [_, _, _, _, _, _, _, _] =
                    elvis_core_apply_rule(Config,
                                          elvis_style,
                                          atom_naming_convention,
                                          #{regex => KeepRegex,
                                            enclosed_atoms => "^([\\\\][\-a-z0-9A-Z_' \\\\]*)$"},
                                          FailPath);
            erl_files ->
                [_, _, _, _, _, _, _, _, _] =
                    elvis_core_apply_rule(Config,
                                          elvis_style,
                                          atom_naming_convention,
                                          #{regex => KeepRegex,
                                            enclosed_atoms => "^([\\\\][\-a-z0-9A-Z_' \\\\]*)$"},
                                          FailPath)
        end.

-spec verify_no_throw(config()) -> any().
verify_no_throw(Config) ->
    _Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % fail
    FailModule = fail_no_throw,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    [_, _, _, _] = elvis_core_apply_rule(Config, elvis_style, no_throw, #{}, FailPath).

-spec verify_no_dollar_space(config()) -> any().
verify_no_dollar_space(Config) ->
    _Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % fail
    FailModule = fail_no_dollar_space,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    [_, _] = elvis_core_apply_rule(Config, elvis_style, no_dollar_space, #{}, FailPath).

-spec verify_no_author(config()) -> any().
verify_no_author(Config) ->
    _Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    % fail
    FailModule = fail_no_author,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    [_, _] = elvis_core_apply_rule(Config, elvis_style, no_author, #{}, FailPath).

-spec verify_no_import(config()) -> any().
verify_no_import(Config) ->
    _Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    FailPath = "fail_no_import." ++ Ext,
    [_, _] = elvis_core_apply_rule(Config, elvis_style, no_import, #{}, FailPath).

-spec verify_no_catch_expressions(config()) -> any().
verify_no_catch_expressions(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    FailPath = "fail_no_catch_expressions." ++ Ext,

    R = elvis_core_apply_rule(Config, elvis_style, no_catch_expressions, #{}, FailPath),
    _ = case Group of
            beam_files ->
                [#{info := [10]}, #{info := [21]}, #{info := [21]}] = lists:sort(R);
            erl_files ->
                [#{info := [9]}, #{info := [24]}, #{info := [26]}] = lists:sort(R)
        end.

-spec verify_no_single_clause_case(config()) -> any().
verify_no_single_clause_case(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_no_single_clause_case." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, no_single_clause_case, #{}, PassPath),

    FailPath = "fail_no_single_clause_case." ++ Ext,

    R = elvis_core_apply_rule(Config, elvis_style, no_single_clause_case, #{}, FailPath),
    _ = case Group of
            beam_files ->
                [_, _, _] = R;
            erl_files ->
                [#{line_num := 6}, #{line_num := 14}, #{line_num := 16}] = R
        end.

-spec verify_no_match_in_condition(config()) -> any().
verify_no_match_in_condition(Config) ->
    Group = proplists:get_value(group, Config, erl_files),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    PassPath = "pass_no_match_in_condition." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, no_match_in_condition, #{}, PassPath),

    FailPath = "fail_no_match_in_condition." ++ Ext,

    R = elvis_core_apply_rule(Config, elvis_style, no_match_in_condition, #{}, FailPath),
    case Group of
        beam_files ->
            [_, _] = R;
        erl_files ->
            [#{line_num := 14}, #{line_num := 22}] = R
    end,
    ok.

-spec verify_numeric_format(config()) -> any().
verify_numeric_format(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    BaseRegex = "^[^_]+$",

    % pass
    PassModule = pass_numeric_format,
    PassPath = atom_to_list(PassModule) ++ "." ++ Ext,

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              numeric_format,
                              #{regex => BaseRegex},
                              PassPath),

    % fail
    FailModule = fail_numeric_format,
    FailPath = atom_to_list(FailModule) ++ "." ++ Ext,

    [_, _, _, _, _, _, _, _, _, _, _, _, _, _, _] = % no underscores
        elvis_core_apply_rule(Config,
                              elvis_style,
                              numeric_format,
                              #{regex => BaseRegex,
                                int_regex => same,
                                float_regex => same},
                              FailPath),
    [_, _, _, _, _, _, _, _, _, _, _] = % with at least 2 digits
        elvis_core_apply_rule(Config,
                              elvis_style,
                              numeric_format,
                              #{regex => "^(.*\\d\\d)$",
                                int_regex => same,
                                float_regex => same},
                              FailPath),
    [_, _, _, _] = % only base 10
        elvis_core_apply_rule(Config,
                              elvis_style,
                              numeric_format,
                              #{regex => "^([^#]+)$",
                                int_regex => same,
                                float_regex => same},
                              FailPath),

    % any float, nothing else - impossible to match base regex
    [_, _, _, _, _, _, _, _, _, _, _, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              numeric_format,
                              #{regex => "impossible-to-match",
                                int_regex => same,
                                float_regex => ".*"},
                              FailPath),

    % any integer, nothing else - impossible to match base regex
    [_, _, _, _] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              numeric_format,
                              #{regex => "impossible-to-match",
                                int_regex => ".*",
                                float_regex => same},
                              FailPath),

    % base regex is ignored
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              numeric_format,
                              #{regex => "impossible-to-match",
                                int_regex => ".*",
                                float_regex => ".*"},
                              FailPath),

    % ignored module
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              numeric_format,
                              #{regex => BaseRegex, ignore => [FailModule]},
                              FailPath),

    % ugly
    UglyModule = ugly_numeric_format,
    UglyPath = atom_to_list(UglyModule) ++ "." ++ Ext,

    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              numeric_format,
                              #{regex => BaseRegex},
                              UglyPath),

    true.

-spec verify_export_used_types(config()) -> any().
verify_export_used_types(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),
    PathPass = "pass_export_used_types." ++ Ext,
    [] = elvis_core_apply_rule(Config, elvis_style, export_used_types, #{}, PathPass),

    PathFail = "fail_export_used_types." ++ Ext,
    [#{line_num := 3}] =
        elvis_core_apply_rule(Config, elvis_style, export_used_types, #{}, PathFail).

-spec verify_private_data_types(config()) -> any().
verify_private_data_types(Config) ->
    Ext = proplists:get_value(test_file_ext, Config, "erl"),
    PathPass = "pass_private_data_types2." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              private_data_types,
                              #{apply_to => [record, map, tuple]},
                              PathPass),
    PathPass2 = "pass_private_data_types2." ++ Ext,
    [] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              private_data_types,
                              #{apply_to => [record, map, tuple]},
                              PathPass2),
    % Default applies only to records
    PathFail = "fail_private_data_types." ++ Ext,
    [#{line_num := _}] =
        elvis_core_apply_rule(Config, elvis_style, private_data_types, #{}, PathFail),
    [#{line_num := _}] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              private_data_types,
                              #{apply_to => [tuple]},
                              PathFail),
    [#{line_num := _}] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              private_data_types,
                              #{apply_to => [map]},
                              PathFail),
    [#{line_num := _}, #{line_num := _}, #{line_num := _}] =
        elvis_core_apply_rule(Config,
                              elvis_style,
                              private_data_types,
                              #{apply_to => [record, tuple, map]},
                              PathFail).

-spec results_are_ordered_by_line(config()) -> true.
results_are_ordered_by_line(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    {fail, Results} = elvis_core:rock(ElvisConfig),
    true = lists:all(fun(X) -> X end, is_item_line_sort(Results)).

-spec oddities(config()) -> true.
oddities(_Config) ->
    ElvisConfig =
        [#{dirs => ["../../_build/test/lib/elvis_core/test/examples"],
           filter => "odditiesß.erl",
           ruleset => erl_files,
           rules => [{elvis_style, god_modules, #{limit => 0}}]}],
    {fail, [#{rules := [_, _, _, _]}]} = elvis_core:rock(ElvisConfig),
    true.

-spec verify_elvis_attr_atom_naming_convention(config()) -> true.
verify_elvis_attr_atom_naming_convention(Config) ->
    verify_elvis_attr(Config, "pass_atom_naming_convention_elvis_attr").

-spec verify_elvis_attr_numeric_format(config()) -> true.
verify_elvis_attr_numeric_format(Config) ->
    verify_elvis_attr(Config, "pass_numeric_format_elvis_attr").

-spec verify_elvis_attr_dont_repeat_yourself(config()) -> true.
verify_elvis_attr_dont_repeat_yourself(Config) ->
    verify_elvis_attr(Config, "pass_dont_repeat_yourself_elvis_attr").

-spec verify_elvis_attr_function_naming_convention(config()) -> true.
verify_elvis_attr_function_naming_convention(Config) ->
    verify_elvis_attr(Config, "pass_function_naming_convention_elvis_attr").

-spec verify_elvis_attr_god_modules(config()) -> true.
verify_elvis_attr_god_modules(Config) ->
    verify_elvis_attr(Config, "pass_god_modules_elvis_attr").

-spec verify_elvis_attr_invalid_dynamic_call(config()) -> true.
verify_elvis_attr_invalid_dynamic_call(Config) ->
    verify_elvis_attr(Config, "pass_invalid_dynamic_call_elvis_attr").

-spec verify_elvis_attr_line_length(config()) -> true.
verify_elvis_attr_line_length(Config) ->
    verify_elvis_attr(Config, "pass_line_length_elvis_attr").

-spec verify_elvis_attr_macro_module_names(config()) -> true.
verify_elvis_attr_macro_module_names(Config) ->
    verify_elvis_attr(Config, "pass_macro_module_names_elvis_attr").

-spec verify_elvis_attr_macro_names(config()) -> true.
verify_elvis_attr_macro_names(Config) ->
    verify_elvis_attr(Config, "pass_macro_names_elvis_attr").

-spec verify_elvis_attr_max_function_arity(config()) -> true.
verify_elvis_attr_max_function_arity(Config) ->
    verify_elvis_attr(Config, "pass_max_function_arity_elvis_attr").

-spec verify_elvis_attr_max_anonymous_function_arity(config()) -> true.
verify_elvis_attr_max_anonymous_function_arity(Config) ->
    verify_elvis_attr(Config, "pass_max_anonymous_function_arity_elvis_attr").

-spec verify_elvis_attr_max_function_length(config()) -> true.
verify_elvis_attr_max_function_length(Config) ->
    verify_elvis_attr(Config, "pass_max_function_length_elvis_attr").

-spec verify_elvis_attr_max_module_length(config()) -> true.
verify_elvis_attr_max_module_length(Config) ->
    verify_elvis_attr(Config, "pass_max_module_length_elvis_attr").

-spec verify_elvis_attr_module_naming_convention(config()) -> true.
verify_elvis_attr_module_naming_convention(Config) ->
    verify_elvis_attr(Config, "pass_module_naming-convention_elvis_attr").

-spec verify_elvis_attr_nesting_level(config()) -> true.
verify_elvis_attr_nesting_level(Config) ->
    verify_elvis_attr(Config, "pass_nesting_level_elvis_attr").

-spec verify_elvis_attr_no_behavior_info(config()) -> true.
verify_elvis_attr_no_behavior_info(Config) ->
    verify_elvis_attr(Config, "pass_no_behavior_info_elvis_attr").

-spec verify_elvis_attr_no_call(config()) -> true.
verify_elvis_attr_no_call(Config) ->
    verify_elvis_attr(Config, "pass_no_call_elvis_attr").

-spec verify_elvis_attr_no_debug_call(config()) -> true.
verify_elvis_attr_no_debug_call(Config) ->
    verify_elvis_attr(Config, "pass_no_debug_call_elvis_attr").

-spec verify_elvis_attr_no_if_expression(config()) -> true.
verify_elvis_attr_no_if_expression(Config) ->
    verify_elvis_attr(Config, "pass_no_if_expression_elvis_attr").

-spec verify_elvis_attr_no_nested_try_catch(config()) -> true.
verify_elvis_attr_no_nested_try_catch(Config) ->
    verify_elvis_attr(Config, "pass_no_nested_try_catch_elvis_attr").

-spec verify_elvis_attr_no_successive_maps(config()) -> true.
verify_elvis_attr_no_successive_maps(Config) ->
    verify_elvis_attr(Config, "pass_no_successive_maps_elvis_attr").

-spec verify_elvis_attr_no_spec_with_records(config()) -> true.
verify_elvis_attr_no_spec_with_records(Config) ->
    verify_elvis_attr(Config, "pass_no_spec_with_records_elvis_attr").

-spec verify_elvis_attr_no_tabs(config()) -> true.
verify_elvis_attr_no_tabs(Config) ->
    verify_elvis_attr(Config, "pass_no_tabs_elvis_attr").

-spec verify_elvis_attr_no_trailing_whitespace(config()) -> true.
verify_elvis_attr_no_trailing_whitespace(Config) ->
    verify_elvis_attr(Config, "pass_no_trailing_whitespace_elvis_attr").

-spec verify_elvis_attr_operator_spaces(config()) -> true.
verify_elvis_attr_operator_spaces(Config) ->
    verify_elvis_attr(Config, "pass_operator_spaces_elvis_attr").

-spec verify_elvis_attr_state_record_and_type(config()) -> true.
verify_elvis_attr_state_record_and_type(Config) ->
    verify_elvis_attr(Config, "pass_state_record_and_type_elvis_attr").

-spec verify_elvis_attr_used_ignored_variable(config()) -> true.
verify_elvis_attr_used_ignored_variable(Config) ->
    verify_elvis_attr(Config, "pass_used_ignored_variable_elvis_attr").

-spec verify_elvis_attr_variable_naming_convention(config()) -> true.
verify_elvis_attr_variable_naming_convention(Config) ->
    verify_elvis_attr(Config, "pass_variable_naming_convention_elvis_attr").

-spec verify_elvis_attr_behaviour_spelling(config()) -> true.
verify_elvis_attr_behaviour_spelling(Config) ->
    verify_elvis_attr(Config, "pass_behaviour_spelling_elvis_attr").

-spec verify_elvis_attr_param_pattern_matching(config()) -> true.
verify_elvis_attr_param_pattern_matching(Config) ->
    verify_elvis_attr(Config, "pass_param_pattern_matching_elvis_attr").

-spec verify_elvis_attr_private_data_types(config()) -> true.
verify_elvis_attr_private_data_types(Config) ->
    verify_elvis_attr(Config, "pass_private_data_types_elvis_attr").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elvis_core_apply_rule(Config, Module, Function, RuleConfig, Filename) ->
    ElvisConfig =
        elvis_test_utils:config(
            proplists:get_value(group, Config, erl_files)),
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

verify_elvis_attr(Config, FilenameNoExt) ->
    ElvisConfig =
        elvis_test_utils:config(
            proplists:get_value(group, Config, erl_files)),
    SrcDirs = elvis_config:dirs(ElvisConfig),
    Ext = proplists:get_value(test_file_ext, Config, "erl"),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, FilenameNoExt ++ "." ++ Ext),

    {ok, #{rules := RuleResults}} = elvis_core:do_rock(File, ElvisConfig),
    [[] = Items || #{items := Items} <- RuleResults],
    true.

-spec is_item_line_sort([elvis_result:file()]) -> [boolean()].
is_item_line_sort(Result) ->
    Items = [Items || #{rules := Rules} <- Result, #{items := Items} <- Rules],
    lists:map(fun is_list_sort/1, Items).

-spec is_list_sort([any()]) -> boolean().
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

-spec assert_length(non_neg_integer(), [any()], atom()) -> any().
assert_length(Expected, List, RuleName) ->
    case length(List) of
        Expected ->
            ok;
        _ ->
            throw({unexpected_response_length, RuleName, {expected, Expected}, {got, List}})
    end.
