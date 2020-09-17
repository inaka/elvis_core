-module(elvis_rulesets).

-export([rules/1]).

-spec rules(Group::atom()) -> list().
rules(erl_files) ->
    lists:map(
        fun (Rule) ->
            {elvis_style, Rule, elvis_style:default(Rule)}
        end,
        [ line_length
        , no_tabs
        , no_trailing_whitespace
        , macro_names
        , macro_module_names
        , operator_spaces
        , nesting_level
        , god_modules
        , no_if_expression
        , invalid_dynamic_call
        , used_ignored_variable
        , no_behavior_info
        , module_naming_convention
        , function_naming_convention
        , state_record_and_type
        , no_spec_with_records
        , dont_repeat_yourself
        , no_debug_call
        , variable_naming_convention
        , no_nested_try_catch
        , atom_naming_convention ]);
rules(makefiles) ->
  [ {elvis_project, no_deps_master_erlang_mk, #{ignore => []}}
  , {elvis_project, protocol_for_deps_erlang_mk, #{ignore => []}}
  ];
rules(rebar_config) ->
  [ {elvis_project, no_deps_master_rebar, #{ignore => []}}
  , {elvis_project, protocol_for_deps_rebar, #{ignore => []}}
  ];
rules(elvis_config) ->
  [{elvis_project, old_configuration_format}];
rules(_Group) -> [].
