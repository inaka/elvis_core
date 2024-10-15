-module(elvis_rulesets).

-format #{inline_items => none}.

-export([rules/1, set_rulesets/1]).

-spec set_rulesets(#{atom() => list()}) -> ok.
set_rulesets(RuleSets) ->
    Tid = ensure_clean_table(),
    lists:foreach(fun({Name, Rules}) -> true = ets:insert(Tid, {Name, Rules}) end,
                  maps:to_list(RuleSets)).

-spec rules(Group :: atom()) -> [elvis_core:rule()].
rules(gitignore) ->
    lists:map(fun({Mod, Rule}) -> {Mod, Rule, apply(Mod, default, [Rule])} end,
              [{elvis_gitignore, Rule} || Rule <- [required_patterns, forbidden_patterns]]);
rules(hrl_files) ->
    lists:map(fun({Mod, Rule}) -> {Mod, Rule, apply(Mod, default, [Rule])} end,
              [{elvis_text_style, Rule} || Rule <- [line_length, no_tabs, no_trailing_whitespace]]
              ++ [{elvis_style, Rule}
                  || Rule
                         <- [atom_naming_convention,
                             consistent_variable_casing,
                             macro_module_names,
                             macro_names,
                             nesting_level,
                             no_author,
                             no_behavior_info,
                             no_block_expressions,
                             no_catch_expressions,
                             no_debug_call,
                             no_dollar_space,
                             no_if_expression,
                             no_import,
                             no_match_in_condition,
                             no_nested_try_catch,
                             no_single_clause_case,
                             no_space,
                             no_space_after_pound,
                             no_specs,
                             no_successive_maps,
                             no_throw,
                             no_types,
                             numeric_format,
                             operator_spaces,
                             used_ignored_variable,
                             variable_naming_convention]]);
rules(erl_files) ->
    lists:map(fun({Mod, Rule}) -> {Mod, Rule, apply(Mod, default, [Rule])} end,
              [{elvis_text_style, Rule} || Rule <- [line_length, no_tabs, no_trailing_whitespace]]
              ++ [{elvis_style, Rule}
                  || Rule
                         <- [atom_naming_convention,
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
                             no_catch_expressions,
                             no_debug_call,
                             no_dollar_space,
                             no_if_expression,
                             no_import,
                             no_match_in_condition,
                             no_nested_try_catch,
                             no_single_clause_case,
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
                             variable_naming_convention]]);
rules(erl_files_strict) ->
    rules(erl_files)
    ++ lists:map(fun({Mod, Rule}) -> {Mod, Rule, apply(Mod, default, [Rule])} end,
                 [{elvis_style, Rule}
                  || Rule
                         <- [always_shortcircuit,
                             consistent_generic_type,
                             max_function_length,
                             max_module_length,
                             no_call,
                             no_init_lists,
                             no_common_caveats_call,
                             no_macros,
                             state_record_and_type]]);
rules(beam_files) ->
    lists:map(fun(Rule) -> {elvis_style, Rule, elvis_style:default(Rule)} end,
              [atom_naming_convention,
               behaviour_spelling,
               consistent_variable_casing,
               dont_repeat_yourself,
               export_used_types,
               function_naming_convention,
               god_modules,
               invalid_dynamic_call,
               max_anonymous_function_arity,
               max_function_arity,
               module_naming_convention,
               nesting_level,
               no_author,
               no_catch_expressions,
               no_debug_call,
               no_if_expression,
               no_import,
               no_init_lists,
               no_match_in_condition,
               no_nested_try_catch,
               no_single_clause_case,
               no_spec_with_records,
               no_successive_maps,
               no_throw,
               param_pattern_matching,
               private_data_types,
               used_ignored_variable,
               variable_naming_convention]);
rules(rebar_config) ->
    lists:map(fun(Rule) -> {elvis_project, Rule, elvis_project:default(Rule)} end,
              [no_branch_deps, protocol_for_deps]);
rules(elvis_config) ->
    lists:map(fun(Rule) -> {elvis_project, Rule, elvis_project:default(Rule)} end,
              [old_configuration_format]);
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
