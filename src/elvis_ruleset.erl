-module(elvis_ruleset).

-format(#{inline_items => none}).

-export([rules/1, set_rulesets/1]).

-spec set_rulesets(#{atom() => list()}) -> ok.
set_rulesets(Rulesets) ->
    Tid = ensure_clean_table(),
    lists:foreach(
        fun({Ruleset, NSNameDefs}) ->
            Rules = [elvis_rule:from_tuple(NSNameDef) || NSNameDef <- NSNameDefs],
            true = ets:insert(Tid, {Ruleset, Rules})
        end,
        maps:to_list(Rulesets)
    ).

-spec rules(Group :: atom()) -> [elvis_rule:t()].
rules(gitignore) ->
    gitignore_rules();
rules(hrl_files) ->
    hrl_files_rules();
rules(hrl_files_strict) ->
    hrl_files_strict_rules();
rules(erl_files) ->
    erl_files_rules();
rules(erl_files_strict) ->
    erl_files_strict_rules();
rules(beam_files) ->
    beam_files_rules();
rules(beam_files_strict) ->
    beam_files_strict_rules();
rules(rebar_config) ->
    rebar_config_rules();
rules(erl_files_test) ->
    erl_files_test_rules();
rules(Group) ->
    try
        ets:lookup_element(?MODULE, Group, 2)
    catch
        error:badarg ->
            []
    end.

ensure_clean_table() ->
    case ets:info(?MODULE) of
        undefined ->
            ets:new(?MODULE, [set, named_table, {keypos, 1}, public]);
        _ ->
            true = ets:delete_all_objects(?MODULE),
            ?MODULE
    end.

gitignore_rules() ->
    [
        elvis_rule:new(elvis_gitignore, forbidden_patterns),
        elvis_rule:new(elvis_gitignore, required_patterns)
    ].

hrl_files_rules() ->
    trim(erl_files_rules() ++ hrl_only_files_rules(), doesnt_work_on_hrl_files()).

hrl_only_files_rules() ->
    [
        elvis_rule:new(elvis_style, no_includes),
        elvis_rule:new(elvis_style, no_specs),
        elvis_rule:new(elvis_style, no_types)
    ].

hrl_files_strict_rules() ->
    trim(erl_files_strict_rules(), doesnt_work_on_hrl_files()).

doesnt_work_on_hrl_files() ->
    [
        elvis_rule:new(elvis_style, behaviour_spelling),
        elvis_rule:new(elvis_style, dont_repeat_yourself),
        elvis_rule:new(elvis_style, export_used_types),
        elvis_rule:new(elvis_style, function_naming_convention),
        elvis_rule:new(elvis_style, no_god_modules),
        elvis_rule:new(elvis_style, no_invalid_dynamic_calls),
        elvis_rule:new(elvis_style, max_anonymous_function_arity),
        elvis_rule:new(elvis_style, max_anonymous_function_clause_length),
        elvis_rule:new(elvis_style, max_anonymous_function_length),
        elvis_rule:new(elvis_style, max_function_clause_length),
        elvis_rule:new(elvis_style, max_function_length),
        elvis_rule:new(elvis_style, ms_transform_included),
        elvis_rule:new(elvis_style, no_call),
        elvis_rule:new(elvis_style, no_common_caveats_call),
        elvis_rule:new(elvis_style, no_init_lists),
        elvis_rule:new(elvis_style, no_macros),
        elvis_rule:new(elvis_style, no_spec_with_records),
        elvis_rule:new(elvis_style, param_pattern_matching),
        elvis_rule:new(elvis_style, private_data_types),
        elvis_rule:new(elvis_style, state_record_and_type)
    ].

erl_files_rules() ->
    elvis_style_rules() ++ elvis_text_style_rules().

elvis_style_rules() ->
    [
        elvis_rule:new(elvis_style, atom_naming_convention),
        elvis_rule:new(elvis_style, behaviour_spelling),
        elvis_rule:new(elvis_style, variable_casing),
        elvis_rule:new(elvis_style, dont_repeat_yourself),
        elvis_rule:new(elvis_style, export_used_types),
        elvis_rule:new(elvis_style, function_naming_convention),
        elvis_rule:new(elvis_style, no_god_modules),
        elvis_rule:new(elvis_style, no_invalid_dynamic_calls),
        elvis_rule:new(elvis_style, macro_naming_convention),
        elvis_rule:new(elvis_style, max_anonymous_function_arity),
        elvis_rule:new(elvis_style, max_function_arity),
        elvis_rule:new(elvis_style, module_naming_convention),
        elvis_rule:new(elvis_style, no_deep_nesting),
        elvis_rule:new(elvis_style, no_author),
        elvis_rule:new(elvis_style, no_behavior_info),
        elvis_rule:new(elvis_style, no_block_expressions),
        elvis_rule:new(elvis_style, no_boolean_in_comparison),
        elvis_rule:new(elvis_style, no_catch_expressions),
        elvis_rule:new(elvis_style, no_debug_call),
        elvis_rule:new(elvis_style, no_dollar_space),
        elvis_rule:new(elvis_style, no_if_expression),
        elvis_rule:new(elvis_style, no_import),
        elvis_rule:new(elvis_style, no_match_in_condition),
        elvis_rule:new(elvis_style, no_nested_try_catch),
        elvis_rule:new(elvis_style, no_operation_on_same_value),
        elvis_rule:new(elvis_style, no_receive_without_timeout),
        elvis_rule:new(elvis_style, no_single_clause_case),
        elvis_rule:new(elvis_style, no_single_match_maybe),
        elvis_rule:new(elvis_style, no_space_after_pound),
        elvis_rule:new(elvis_style, no_space),
        elvis_rule:new(elvis_style, no_spec_with_records),
        elvis_rule:new(elvis_style, no_successive_maps),
        elvis_rule:new(elvis_style, no_throw),
        elvis_rule:new(elvis_style, numeric_format),
        elvis_rule:new(elvis_style, operator_spaces),
        elvis_rule:new(elvis_style, param_pattern_matching),
        elvis_rule:new(elvis_style, private_data_types),
        elvis_rule:new(elvis_style, no_used_ignored_variables),
        elvis_rule:new(elvis_style, variable_naming_convention),
        elvis_rule:new(elvis_style, guard_operators),
        elvis_rule:new(elvis_style, simplify_anonymous_functions)
    ].

erl_files_test_rules() ->
    trim(
        [
            elvis_rule:new(elvis_style, dont_repeat_yourself),
            elvis_rule:new(elvis_style, no_god_modules)
        ],
        elvis_style_rules()
    ).

elvis_text_style_rules() ->
    [
        elvis_rule:new(elvis_text_style, line_length),
        elvis_rule:new(elvis_text_style, no_tabs),
        elvis_rule:new(elvis_text_style, no_trailing_whitespace)
    ].

erl_files_strict_rules() ->
    erl_files_rules() ++ elvis_style_stricter_rules() ++ elvis_text_style_stricter_rules().

elvis_style_stricter_rules() ->
    [
        elvis_rule:new(elvis_style, always_shortcircuit),
        elvis_rule:new(elvis_style, generic_type),
        elvis_rule:new(elvis_style, max_anonymous_function_clause_length),
        elvis_rule:new(elvis_style, max_anonymous_function_length),
        elvis_rule:new(elvis_style, max_function_clause_length),
        elvis_rule:new(elvis_style, max_function_length),
        elvis_rule:new(elvis_style, max_module_length),
        elvis_rule:new(elvis_style, ms_transform_included),
        elvis_rule:new(elvis_style, no_call),
        elvis_rule:new(elvis_style, no_common_caveats_call),
        elvis_rule:new(elvis_style, no_init_lists),
        elvis_rule:new(elvis_style, no_macros),
        elvis_rule:new(elvis_style, prefer_unquoted_atoms),
        elvis_rule:new(elvis_style, state_record_and_type),
        elvis_rule:new(elvis_style, prefer_include)
    ].

elvis_text_style_stricter_rules() ->
    [
        elvis_rule:new(elvis_text_style, no_redundant_blank_lines)
    ].

beam_files_rules() ->
    trim(erl_files_rules(), doesnt_work_on_beam_files()).

beam_files_strict_rules() ->
    trim(erl_files_strict_rules(), doesnt_work_on_beam_files()).

doesnt_work_on_beam_files() ->
    not_on_beam() ++ elvis_text_style_rules() ++ elvis_text_style_stricter_rules().

not_on_beam() ->
    [
        elvis_rule:new(elvis_style, macro_naming_convention),
        elvis_rule:new(elvis_style, max_anonymous_function_clause_length),
        elvis_rule:new(elvis_style, max_anonymous_function_length),
        elvis_rule:new(elvis_style, max_function_clause_length),
        elvis_rule:new(elvis_style, max_function_length),
        elvis_rule:new(elvis_style, max_module_length),
        elvis_rule:new(elvis_style, no_dollar_space),
        elvis_rule:new(elvis_style, no_macros),
        elvis_rule:new(elvis_style, no_space_after_pound),
        elvis_rule:new(elvis_style, no_space),
        elvis_rule:new(elvis_style, numeric_format),
        elvis_rule:new(elvis_style, operator_spaces)
    ].

rebar_config_rules() ->
    [
        elvis_rule:new(elvis_project, no_branch_deps),
        elvis_rule:new(elvis_project, protocol_for_deps)
    ].

trim(Ruleset, RulesToRemove) ->
    lists:filter(
        fun(Rule) ->
            lists:all(
                fun(RuleToRemove) ->
                    not elvis_rule:same(RuleToRemove, Rule)
                end,
                RulesToRemove
            )
        end,
        Ruleset
    ).
