-module(elvis_rulesets).

-format #{inline_items => none}.

-export([rules/1, set_rulesets/1]).

-spec set_rulesets(#{atom() => list()}) -> ok.
set_rulesets(RuleSets) ->
    Tid = ensure_clean_table(),
    lists:foreach(fun({Name, Rules}) -> true = ets:insert(Tid, {Name, Rules}) end,
                  maps:to_list(RuleSets)).

-spec rules(Group :: atom()) -> [elvis_core:rule()].
rules(hrl_files) ->
    lists:map(fun({Mod, Rule}) -> {Mod, Rule, apply(Mod, default, [Rule])} end,
              [{elvis_text_style, line_length},
               {elvis_text_style, no_tabs},
               {elvis_text_style, no_trailing_whitespace},
               {elvis_style, macro_names},
               {elvis_style, macro_module_names},
               {elvis_style, no_block_expressions},
               {elvis_style, operator_spaces},
               {elvis_style, no_space_after_pound},
               {elvis_style, no_space},
               {elvis_style, nesting_level},
               {elvis_style, no_if_expression},
               {elvis_style, used_ignored_variable},
               {elvis_style, no_behavior_info},
               {elvis_style, no_debug_call},
               {elvis_style, variable_naming_convention},
               {elvis_style, consistent_variable_casing},
               {elvis_style, no_nested_try_catch},
               {elvis_style, no_successive_maps},
               {elvis_style, atom_naming_convention},
               {elvis_style, no_throw},
               {elvis_style, no_dollar_space},
               {elvis_style, no_author},
               {elvis_style, no_import},
               {elvis_style, no_catch_expressions},
               {elvis_style, no_single_clause_case},
               {elvis_style, no_match_in_condition},
               {elvis_style, numeric_format},
               {elvis_style, no_specs},
               {elvis_style, no_types}]);
rules(erl_files) ->
    lists:map(fun({Mod, Rule}) -> {Mod, Rule, apply(Mod, default, [Rule])} end,
              [{elvis_text_style, line_length},
               {elvis_text_style, no_tabs},
               {elvis_text_style, no_trailing_whitespace},
               {elvis_style, macro_names},
               {elvis_style, macro_module_names},
               {elvis_style, no_block_expressions},
               {elvis_style, operator_spaces},
               {elvis_style, no_space_after_pound},
               {elvis_style, no_space},
               {elvis_style, nesting_level},
               {elvis_style, god_modules},
               {elvis_style, no_if_expression},
               {elvis_style, invalid_dynamic_call},
               {elvis_style, used_ignored_variable},
               {elvis_style, no_behavior_info},
               {elvis_style, module_naming_convention},
               {elvis_style, function_naming_convention},
               {elvis_style, no_spec_with_records},
               {elvis_style, dont_repeat_yourself},
               {elvis_style, no_debug_call},
               {elvis_style, variable_naming_convention},
               {elvis_style, consistent_variable_casing},
               {elvis_style, no_nested_try_catch},
               {elvis_style, no_successive_maps},
               {elvis_style, atom_naming_convention},
               {elvis_style, no_throw},
               {elvis_style, no_dollar_space},
               {elvis_style, no_author},
               {elvis_style, no_import},
               {elvis_style, no_catch_expressions},
               {elvis_style, no_single_clause_case},
               {elvis_style, no_match_in_condition},
               {elvis_style, numeric_format},
               {elvis_style, behaviour_spelling},
               {elvis_style, export_used_types},
               {elvis_style, max_function_arity},
               {elvis_style, max_anonymous_function_arity}]);
rules(beam_files) ->
    lists:map(fun(Rule) -> {elvis_style, Rule, elvis_style:default(Rule)} end,
              [nesting_level,
               god_modules,
               no_if_expression,
               invalid_dynamic_call,
               used_ignored_variable,
               module_naming_convention,
               function_naming_convention,
               no_spec_with_records,
               dont_repeat_yourself,
               no_debug_call,
               variable_naming_convention,
               consistent_variable_casing,
               no_nested_try_catch,
               no_successive_maps,
               atom_naming_convention,
               no_throw,
               no_author,
               no_import,
               no_catch_expressions,
               no_single_clause_case,
               no_match_in_condition,
               behaviour_spelling,
               export_used_types,
               max_function_arity,
               max_anonymous_function_arity]);
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
