-module(elvis_rulesets).

-export([rules/1]).

-spec rules(Group::atom()) -> list().
rules(erl_files) ->
  [ {elvis_style, line_length, #{limit => 100, skip_comments => false}}
  , {elvis_style, no_tabs}
  , {elvis_style, no_trailing_whitespace, #{ignore_empty_lines => false}}
  , {elvis_style, macro_names}
  , {elvis_style, macro_module_names}
  , { elvis_style
    , operator_spaces
    , #{rules => [{right, ","}, {right, "++"}, {left, "++"}]}
    }
  , {elvis_style, nesting_level, #{level => 4, ignore => []}}
  , {elvis_style, god_modules, #{limit => 25, ignore => []}}
  , {elvis_style, no_if_expression}
  , {elvis_style, invalid_dynamic_call, #{ignore => []}}
  , {elvis_style, used_ignored_variable, #{ignore => []}}
  , {elvis_style, no_behavior_info}
  , { elvis_style
    , module_naming_convention
    , #{regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$", ignore => []}
    }
  , { elvis_style
    , function_naming_convention
    , #{regex => "^([a-z][a-z0-9]*_?)*$", ignore => []}
    }
  , {elvis_style, state_record_and_type}
  , {elvis_style, no_spec_with_records}
  , {elvis_style, dont_repeat_yourself, #{min_complexity => 10}}
  , {elvis_style, no_debug_call, #{ignore => []}}
  , { elvis_style
    , variable_naming_convention
    , #{regex => "^_?([A-Z][0-9a-zA-Z]*)$", ignore => []}
    }
  , {elvis_style, no_nested_try_catch, #{ignore => []}}
  , {elvis_style, atom_naming_convention, #{regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$", ignore => []}}
  ];
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
