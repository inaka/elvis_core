-module(elvis_rulesets).

-export([rules/1,
         register_ruleset/2]).

-define(RULESET_TABLE, elvis_custom_rulesets).

-spec register_ruleset(Name :: atom(), Rules :: list()) -> true.
register_ruleset(Name, Rules) ->
    Tid = ensure_table(?RULESET_TABLE),
    true = ets:insert(Tid, {Name, Rules}).

-spec rules(Group::atom()) -> [elvis_core:rule()].
rules(erl_files) ->
    lists:map(
        fun ({Mod, Rule}) ->
            {Mod, Rule, Mod:default(Rule)}
        end,
        [ {elvis_text_style, line_length}
        , {elvis_text_style, no_tabs}
        , {elvis_text_style, no_trailing_whitespace}
        , {elvis_style, macro_names}
        , {elvis_style, macro_module_names}
        , {elvis_style, operator_spaces}
        , {elvis_style, nesting_level}
        , {elvis_style, god_modules}
        , {elvis_style, no_if_expression}
        , {elvis_style, invalid_dynamic_call}
        , {elvis_style, used_ignored_variable}
        , {elvis_style, no_behavior_info}
        , {elvis_style, module_naming_convention}
        , {elvis_style, function_naming_convention}
        , {elvis_style, state_record_and_type}
        , {elvis_style, no_spec_with_records}
        , {elvis_style, dont_repeat_yourself}
        , {elvis_style, no_debug_call}
        , {elvis_style, variable_naming_convention}
        , {elvis_style, no_nested_try_catch}
        , {elvis_style, atom_naming_convention}
        ]);
rules(makefiles) ->
    lists:map(
        fun (Rule) ->
            {elvis_project, Rule, elvis_project:default(Rule)}
        end,
        [ no_deps_master_erlang_mk
        , protocol_for_deps_erlang_mk
        ]);
rules(rebar_config) ->
    lists:map(
        fun (Rule) ->
            {elvis_project, Rule, elvis_project:default(Rule)}
        end,
        [ no_deps_master_rebar
        , protocol_for_deps_rebar
        ]);
rules(elvis_config) ->
    lists:map(
        fun (Rule) ->
            {elvis_project, Rule, elvis_project:default(Rule)}
        end,
        [ old_configuration_format
        ]);
rules(Group) ->
    try ets:lookup_element(?RULESET_TABLE, Group, 2) of
        Rules -> Rules
    catch
        error:badarg -> []
    end.

ensure_table(Name) ->
    case ets:info(Name) of
        undefined ->
            ets:new(Name, [set, named_table, {keypos, 1}]);
        _Info ->
            Name
    end.
