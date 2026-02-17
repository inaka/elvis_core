-module(elvis_deprecated_rules).

-elvis([
    {elvis_style, abc_size, #{ignore => [{elvis_deprecated_rules, find, 1}]}},
    {elvis_style, code_complexity, #{ignore => [{elvis_deprecated_rules, find, 1}]}}
]).

-export([find/2]).

%% @doc Returns deprecation info for known deprecated or renamed rules.
%% Returns {deprecated, Message} | {renamed, Message} | valid
-spec find(module(), atom()) -> {deprecated | renamed, string()} | valid.

%% Deprecated rules (no longer have implementations)
find(elvis_style, macro_module_names) ->
    {deprecated, "Rule 'elvis_style:macro_module_names' is deprecated since 4.1.0."};
find(elvis_project, no_deps_master_erlang_mk) ->
    {deprecated,
        "Rule 'elvis_project:no_deps_master_erlang_mk' is deprecated;"
        " use 'no_branch_deps' instead."};
find(elvis_project, no_deps_master_rebar) ->
    {deprecated,
        "Rule 'elvis_project:no_deps_master_rebar' is deprecated;"
        " use 'no_branch_deps' instead."};
find(elvis_project, old_configuration_format) ->
    {deprecated, "Rule 'elvis_project:old_configuration_format' is deprecated."};
find(elvis_project, protocol_for_deps_rebar) ->
    {deprecated,
        "Rule 'elvis_project:protocol_for_deps_rebar' is deprecated;"
        " use 'protocol_for_deps' instead."};
find(elvis_project, protocol_for_deps_erlang_mk) ->
    {deprecated, "Rule 'elvis_project:protocol_for_deps_erlang_mk' is deprecated."};
%% Renamed rules (from PR #505)
find(elvis_style, god_modules) ->
    {renamed, "Rule 'elvis_style:god_modules' has been renamed to 'no_god_modules'."};
find(elvis_style, nesting_level) ->
    {renamed, "Rule 'elvis_style:nesting_level' has been renamed to 'no_deep_nesting'."};
find(elvis_style, invalid_dynamic_call) ->
    {renamed,
        "Rule 'elvis_style:invalid_dynamic_call' has been renamed to 'no_invalid_dynamic_calls'."};
find(elvis_style, used_ignored_variable) ->
    {
        renamed,
        "Rule 'elvis_style:used_ignored_variable' has been renamed to 'no_used_ignored_variables'."
    };
find(elvis_style, macro_names) ->
    {renamed, "Rule 'elvis_style:macro_names' has been renamed to 'macro_naming_convention'."};
find(elvis_style, consistent_generic_type) ->
    {renamed, "Rule 'elvis_style:consistent_generic_type' has been renamed to 'generic_type'."};
find(_, _) ->
    valid.
