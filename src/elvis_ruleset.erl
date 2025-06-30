-module(elvis_ruleset).

-format(#{inline_items => none}).

-export([rules/1, set_rulesets/1]).
-export([option/3]).

-callback default(Rule :: atom()) -> DefaultRuleConfig :: elvis_core:rule_config().

-spec default(RuleNamespace :: module(), Rule :: atom()) ->
    DefaultRuleConfig :: elvis_core:rule_config().
default(RuleNamespace, Rule) ->
    RuleNamespace:default(Rule).

-spec option(OptionName, RuleCfg, Rule) -> OptionValue when
    OptionName :: atom(),
    RuleCfg :: {RuleNamespace, Config, Target, RuleConfig},
    RuleNamespace :: module(),
    Config :: elvis_config:config(),
    Target :: elvis_file:file(),
    RuleConfig :: (Options :: #{atom() => term()}),
    Rule :: atom(),
    OptionValue :: term().
option(OptionName, {RuleNamespace, _Config, _Target, RuleConfig}, Rule) ->
    maybe_default_option(
        maps:get(OptionName, RuleConfig, undefined), OptionName, {RuleNamespace, Rule}
    ).

-spec maybe_default_option(UserDefinedOptionValue, OptionName, RuleNamespaceRule) ->
    OptionValue
when
    UserDefinedOptionValue :: undefined | term(),
    OptionName :: atom(),
    RuleNamespaceRule :: {RuleNamespace, Rule},
    RuleNamespace :: module(),
    Rule :: atom(),
    OptionValue :: term().
maybe_default_option(undefined = _UserDefinedOptionValue, OptionName, {RuleNamespace, Rule}) ->
    maps:get(OptionName, default(RuleNamespace, Rule));
maybe_default_option(UserDefinedOptionValue, _OptionName, _RuleNamespaceRule) ->
    UserDefinedOptionValue.

-spec set_rulesets(#{atom() => list()}) -> ok.
set_rulesets(Rulesets) ->
    Tid = ensure_clean_table(),
    lists:foreach(
        fun({Name, Rules}) -> true = ets:insert(Tid, {Name, Rules}) end,
        maps:to_list(Rulesets)
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
            _ ->
                try
                    ets:lookup_element(?MODULE, Group, 2)
                catch
                    error:badarg ->
                        []
                end
        end,
    lists:map(
        fun({RuleNamespace, Rule}) -> {RuleNamespace, Rule, default(RuleNamespace, Rule)} end,
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
        {elvis_gitignore, forbidden_patterns},
        {elvis_gitignore, required_patterns}
    ].

hrl_files_rules() ->
    (erl_files_rules() ++ hrl_only_files_rules()) -- doesnt_work_on_hrl_files().

hrl_only_files_rules() ->
    [
        {elvis_style, no_nested_hrls},
        {elvis_style, no_specs},
        {elvis_style, no_types}
    ].

hrl_files_strict_rules() ->
    erl_files_strict_rules() -- doesnt_work_on_hrl_files().

doesnt_work_on_hrl_files() ->
    [
        {elvis_style, behaviour_spelling},
        {elvis_style, dont_repeat_yourself},
        {elvis_style, export_used_types},
        {elvis_style, function_naming_convention},
        {elvis_style, no_god_modules},
        {elvis_style, no_invalid_dynamic_calls},
        {elvis_style, max_anonymous_function_arity},
        {elvis_style, max_function_clause_length},
        {elvis_style, max_function_length},
        {elvis_style, ms_transform_included},
        {elvis_style, no_call},
        {elvis_style, no_common_caveats_call},
        {elvis_style, no_init_lists},
        {elvis_style, no_macros},
        {elvis_style, no_spec_with_records},
        {elvis_style, param_pattern_matching},
        {elvis_style, private_data_types},
        {elvis_style, state_record_and_type}
    ].

erl_files_rules() ->
    elvis_style_rules() ++ elvis_text_style_rules().

elvis_style_rules() ->
    [
        {elvis_style, atom_naming_convention},
        {elvis_style, behaviour_spelling},
        {elvis_style, variable_casing},
        {elvis_style, dont_repeat_yourself},
        {elvis_style, export_used_types},
        {elvis_style, function_naming_convention},
        {elvis_style, no_god_modules},
        {elvis_style, no_invalid_dynamic_calls},
        {elvis_style, macro_naming_convention},
        {elvis_style, max_anonymous_function_arity},
        {elvis_style, max_function_arity},
        {elvis_style, module_naming_convention},
        {elvis_style, no_deep_nesting},
        {elvis_style, no_author},
        {elvis_style, no_behavior_info},
        {elvis_style, no_block_expressions},
        {elvis_style, no_boolean_in_comparison},
        {elvis_style, no_catch_expressions},
        {elvis_style, no_debug_call},
        {elvis_style, no_dollar_space},
        {elvis_style, no_if_expression},
        {elvis_style, no_import},
        {elvis_style, no_match_in_condition},
        {elvis_style, no_nested_try_catch},
        {elvis_style, no_operation_on_same_value},
        {elvis_style, no_receive_without_timeout},
        {elvis_style, no_single_clause_case},
        {elvis_style, no_single_match_maybe},
        {elvis_style, no_space_after_pound},
        {elvis_style, no_space},
        {elvis_style, no_spec_with_records},
        {elvis_style, no_successive_maps},
        {elvis_style, no_throw},
        {elvis_style, numeric_format},
        {elvis_style, operator_spaces},
        {elvis_style, param_pattern_matching},
        {elvis_style, private_data_types},
        {elvis_style, no_used_ignored_variables},
        {elvis_style, variable_naming_convention}
    ].

elvis_text_style_rules() ->
    [
        {elvis_text_style, line_length},
        {elvis_text_style, no_tabs},
        {elvis_text_style, no_trailing_whitespace}
    ].

erl_files_strict_rules() ->
    erl_files_rules() ++ elvis_style_stricter_rules() ++ elvis_text_style_stricter_rules().

elvis_style_stricter_rules() ->
    [
        {elvis_style, always_shortcircuit},
        {elvis_style, generic_type},
        {elvis_style, max_function_clause_length},
        {elvis_style, max_function_length},
        {elvis_style, max_module_length},
        {elvis_style, ms_transform_included},
        {elvis_style, no_call},
        {elvis_style, no_common_caveats_call},
        {elvis_style, no_init_lists},
        {elvis_style, no_macros},
        {elvis_style, state_record_and_type}
    ].

elvis_text_style_stricter_rules() ->
    [
        {elvis_text_style, no_redundant_blank_lines},
        {elvis_text_style, prefer_unquoted_atoms}
    ].

beam_files_rules() ->
    erl_files_rules() -- doesnt_work_on_beam_files().

beam_files_strict_rules() ->
    erl_files_strict_rules() -- doesnt_work_on_beam_files().

doesnt_work_on_beam_files() ->
    not_on_beam() ++ elvis_text_style_rules() ++ elvis_text_style_stricter_rules().

not_on_beam() ->
    [
        {elvis_style, macro_naming_convention},
        {elvis_style, max_function_clause_length},
        {elvis_style, max_function_length},
        {elvis_style, max_module_length},
        {elvis_style, no_dollar_space},
        {elvis_style, no_macros},
        {elvis_style, no_space_after_pound},
        {elvis_style, no_space},
        {elvis_style, numeric_format},
        {elvis_style, operator_spaces}
    ].

rebar_config_rules() ->
    [
        {elvis_project, no_branch_deps},
        {elvis_project, protocol_for_deps}
    ].
