-module(elvis_rulesets).

-format(#{inline_items => none}).

-export([rules/1, set_rulesets/1]).

-spec set_rulesets(#{atom() => list()}) -> ok.
set_rulesets(RuleSets) ->
    Tid = ensure_clean_table(),
    lists:foreach(
        fun({Name, Rules}) -> true = ets:insert(Tid, {Name, Rules}) end,
        maps:to_list(RuleSets)
    ).

-spec rules(Group :: atom()) -> [elvis_core:rule()].
rules(Group) ->
    Rules =
        case Group of
            gitignore ->
                gitignore_rules();
            hrl_files ->
                hrl_files_rules();
            hrl_files_strict ->
                hrl_files_strict_rules();
            erl_files ->
                erl_files_rules();
            erl_files_strict ->
                erl_files_strict_rules();
            beam_files ->
                beam_files_rules();
            beam_files_strict ->
                beam_files_strict_rules();
            rebar_config ->
                rebar_config_rules();
            elvis_config ->
                elvis_config_rules();
            _ ->
                try
                    ets:lookup_element(?MODULE, Group, 2)
                catch
                    error:badarg ->
                        []
                end
        end,
    lists:map(
        fun({Mod, Rule}) -> {Mod, Rule, apply(Mod, default, [Rule])} end,
        Rules
    ).

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
        {elvis_gitignore, Rule}
     || Rule <-
            [
                forbidden_patterns,
                required_patterns
            ]
    ].

hrl_files_rules() ->
    (erl_files_rules() ++ hrl_only_files_rules()) -- doesnt_work_on_hrl_files().

hrl_only_files_rules() ->
    [
        {elvis_style, Rule}
     || Rule <-
            [
                no_nested_hrls,
                no_specs,
                no_types
            ]
    ].

hrl_files_strict_rules() ->
    erl_files_strict_rules() -- doesnt_work_on_hrl_files().

doesnt_work_on_hrl_files() ->
    [
        {elvis_style, Rule}
     || Rule <-
            [
                behaviour_spelling,
                dont_repeat_yourself,
                export_used_types,
                function_naming_convention,
                god_modules,
                invalid_dynamic_call,
                max_anonymous_function_arity,
                max_function_clause_length,
                max_function_length,
                ms_transform_included,
                no_call,
                no_common_caveats_call,
                no_init_lists,
                no_macros,
                no_spec_with_records,
                param_pattern_matching,
                private_data_types,
                state_record_and_type
            ]
    ].

erl_files_rules() ->
    elvis_style_rules() ++ elvis_text_style_rules().

elvis_style_rules() ->
    [
        {elvis_style, Rule}
     || Rule <-
            [
                atom_naming_convention,
                behaviour_spelling,
                consistent_variable_casing,
                dont_repeat_yourself,
                export_used_types,
                function_naming_convention,
                god_modules,
                invalid_dynamic_call,
                macro_module_names,
                macro_names,
                max_anonymous_function_arity,
                max_function_arity,
                module_naming_convention,
                nesting_level,
                no_author,
                no_behavior_info,
                no_block_expressions,
                no_boolean_in_comparison,
                no_catch_expressions,
                no_debug_call,
                no_dollar_space,
                no_if_expression,
                no_import,
                no_match_in_condition,
                no_nested_try_catch,
                no_operation_on_same_value,
                no_single_clause_case,
                no_single_match_maybe,
                no_space,
                no_space_after_pound,
                no_spec_with_records,
                no_successive_maps,
                no_throw,
                numeric_format,
                operator_spaces,
                param_pattern_matching,
                private_data_types,
                used_ignored_variable,
                variable_naming_convention
            ]
    ].

elvis_text_style_rules() ->
    [
        {elvis_text_style, Rule}
     || Rule <-
            [
                line_length,
                no_tabs,
                no_trailing_whitespace
            ]
    ].

erl_files_strict_rules() ->
    erl_files_rules() ++ elvis_style_stricter_rules() ++ elvis_text_style_stricter_rules().

elvis_style_stricter_rules() ->
    [
        {elvis_style, Rule}
     || Rule <-
            [
                always_shortcircuit,
                consistent_generic_type,
                max_function_clause_length,
                max_function_length,
                max_module_length,
                ms_transform_included,
                no_call,
                no_common_caveats_call,
                no_init_lists,
                no_macros,
                state_record_and_type
            ]
    ].

elvis_text_style_stricter_rules() ->
    [
        {elvis_text_style, Rule}
     || Rule <-
            [
                no_redundant_blank_lines,
                prefer_unquoted_atoms
            ]
    ].

beam_files_rules() ->
    erl_files_rules() -- doesnt_work_on_beam_files().

beam_files_strict_rules() ->
    erl_files_strict_rules() -- doesnt_work_on_beam_files().

doesnt_work_on_beam_files() ->
    not_on_beam() ++ elvis_text_style_rules() ++ elvis_text_style_stricter_rules().

not_on_beam() ->
    [
        {elvis_style, Rule}
     || Rule <-
            [
                macro_module_names,
                macro_names,
                max_function_clause_length,
                max_function_length,
                max_module_length,
                no_dollar_space,
                no_macros,
                no_space,
                no_space_after_pound,
                numeric_format,
                operator_spaces
            ]
    ].

rebar_config_rules() ->
    [
        {elvis_project, Rule}
     || Rule <-
            [
                no_branch_deps,
                protocol_for_deps
            ]
    ].

elvis_config_rules() ->
    [
        {elvis_project, Rule}
     || Rule <-
            [
                old_configuration_format
            ]
    ].
