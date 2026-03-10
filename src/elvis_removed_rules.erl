-module(elvis_removed_rules).

-elvis([
    {elvis_style, abc_size, #{ignore => [{elvis_removed_rules, find, 1}]}},
    {elvis_style, code_complexity, #{ignore => [{elvis_removed_rules, find, 1}]}}
]).

-export([find/2]).

%% @doc Returns info for known removed or renamed rules.
%% Returns {removed, Message} | {renamed, Message} | valid
-spec find(module(), atom()) -> {removed | renamed, string()} | valid.

%% Removed rules (no longer have implementations)
find(elvis_style, macro_module_names) ->
    {removed, "Rule 'elvis_style:macro_module_names' was removed."};
find(elvis_project, no_deps_master_erlang_mk) ->
    {removed,
        "Rule 'elvis_project:no_deps_master_erlang_mk' was removed;"
        " use 'no_branch_deps' instead."};
find(elvis_project, no_deps_master_rebar) ->
    {removed,
        "Rule 'elvis_project:no_deps_master_rebar' was removed;"
        " use 'no_branch_deps' instead."};
find(elvis_project, old_configuration_format) ->
    {removed, "Rule 'elvis_project:old_configuration_format' was removed."};
find(elvis_project, protocol_for_deps_rebar) ->
    {removed,
        "Rule 'elvis_project:protocol_for_deps_rebar' was removed;"
        " use 'protocol_for_deps' instead."};
find(elvis_project, protocol_for_deps_erlang_mk) ->
    {removed, "Rule 'elvis_project:protocol_for_deps_erlang_mk' was removed."};
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
find(elvis_text_style, line_length) ->
    {renamed, "Rule 'elvis_text_style:line_length' has been renamed to 'max_line_length'."};
find(_, _) ->
    valid.
