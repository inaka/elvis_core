# Rules

In this document, we present links to the rules supported by `elvis` and the available configuration
options for each one. At the end of this document, we share an
[example `elvis.config` file](#example-elvisconfig) you can copy-paste (in your project's root) and
then tweak to your liking.

Rules and other significant changes, like new options (starting from version `0.4.0`), are
identified with `(since ...)` for convenience purposes.

## Avoid, prefer, quick fix

Most, if not all, of the rules will present (opinionated) documentation sections titled
"Avoid" and "Prefer". We aim to provide a "Rationale" with them and, in some cases,
"Exceptions" or "Quick fix", if applicable.

## Style rules

- [Always Shortcircuit](doc_rules/elvis_style/always_shortcircuit.md)
- [Atom Naming Convention](doc_rules/elvis_style/atom_naming_convention.md)
- [Behaviour Spelling](doc_rules/elvis_style/behaviour_spelling.md)
- [Consistent Generic Type](doc_rules/elvis_style/consistent_generic_type.md)
- [Consistent Variable Casing](doc_rules/elvis_style/consistent_variable_casing.md)
- [Don't Repeat Yourself](doc_rules/elvis_style/dont_repeat_yourself.md)
- [Export Used Types](doc_rules/elvis_style/export_used_types.md)
- [Function Naming Convention](doc_rules/elvis_style/function_naming_convention.md)
- [God Modules](doc_rules/elvis_style/god_modules.md)
- [Invalid Dynamic Calls](doc_rules/elvis_style/invalid_dynamic_call.md)
- [Line Length](doc_rules/elvis_text_style/line_length.md)
- [Macro Module Names](doc_rules/elvis_style/macro_module_names.md)
- [Macro Names](doc_rules/elvis_style/macro_names.md)
- [Max Anonymous Function Arity](doc_rules/elvis_style/max_anonymous_function_arity.md)
- [Max Function Arity](doc_rules/elvis_style/max_function_arity.md)
- [Max Function Length](doc_rules/elvis_style/max_function_length.md)
- [Max Function Clause Length](doc_rules/elvis_style/max_function_clause_length.md)
- [Max Module Length](doc_rules/elvis_style/max_module_length.md)
- [Module Naming Convention](doc_rules/elvis_style/module_naming_convention.md)
- [Nesting Level](doc_rules/elvis_style/nesting_level.md)
<!-- markdownlint-disable MD033 -->
- [No <code>&&nbsp;</code>](doc_rules/elvis_style/no_dollar_space.md)
<!-- markdownlint-enable MD033 -->
- [No author](doc_rules/elvis_style/no_author.md)
- [No Behavior Info](doc_rules/elvis_style/no_behavior_info.md)
- [No Block Expressions](doc_rules/elvis_style/no_block_expressions.md)
- [No call](doc_rules/elvis_style/no_call.md)
- [No catch expressions](doc_rules/elvis_style/no_catch_expressions.md)
- [No Common Caveats](doc_rules/elvis_style/no_common_caveats_call.md)
- [No debug call](doc_rules/elvis_style/no_debug_call.md)
- [No If Expression](doc_rules/elvis_style/no_if_expression.md)
- [No Import](doc_rules/elvis_style/no_import.md)
- [No Macros](doc_rules/elvis_style/no_macros.md)
- [No Match in Condition](doc_rules/elvis_style/no_match_in_condition.md)
- [No Nested try...catch Blocks](doc_rules/elvis_style/no_nested_try_catch.md)
- [No Single-Clause Case Statements](doc_rules/elvis_style/no_single_clause_case.md)
- [No Space after #](doc_rules/elvis_style/no_space_after_pound.md)
- [No Space](doc_rules/elvis_style/no_space.md)
- [No Spec With Records](doc_rules/elvis_style/no_spec_with_records.md)
- [No Specs](doc_rules/elvis_style/no_specs.md)
- [No Successive Maps](doc_rules/elvis_style/no_successive_maps.md)
- [No Tabs](doc_rules/elvis_text_style/no_tabs.md)
- [No throw](doc_rules/elvis_style/no_throw.md)
- [No Trailing Whitespace](doc_rules/elvis_text_style/no_trailing_whitespace.md)
- [No Types](doc_rules/elvis_style/no_types.md)
- [Numeric Format](doc_rules/elvis_style/numeric_format.md)
- [Operator Spaces](doc_rules/elvis_style/operator_spaces.md)
- [Param Pattern Matching](doc_rules/elvis_style/param_pattern_matching.md)
- [Private Data Types](doc_rules/elvis_style/private_data_types.md)
- [State Record and Type](doc_rules/elvis_style/state_record_and_type.md)
- [Used Ignored Variable](doc_rules/elvis_style/used_ignored_variable.md)
- [Variable Naming Convention](doc_rules/elvis_style/variable_naming_convention.md)
- [No Init Lists](doc_rules/elvis_style/no_init_lists.md)
- [Prefer Unquoted Atoms](doc_rules/elvis_text_style/prefer_unquoted_atoms.md)
- [ms_transform included](doc_rules/elvis_style/ms_transform_included.md)
- [No Redundant Blank Lines](doc_rules/elvis_text_style/no_redundant_blank_lines.md)
- [No Boolean In Comparison](doc_rules/elvis_style/no_boolean_in_comparison.md)
- [No Operator with Same Values](doc_rules/elvis_style/no_operation_on_same_value.md)

## `.gitignore` rules

- [`.gitignore` required patterns](doc_rules/elvis_gitignore/required_patterns.md)
- [`.gitignore` forbidden patterns](doc_rules/elvis_gitignore/forbidden_patterns.md)

## Project rules

- [No deps master erlang.mk - *deprecated*](doc_rules/elvis_project/no_deps_master_erlang_mk.md)
- [No deps master rebar - *deprecated*](doc_rules/elvis_project/no_deps_master_rebar.md)
- [No deps with branches](doc_rules/elvis_project/no_branch_deps.md)
- [Old configuration format](doc_rules/elvis_project/old_configuration_format.md)
- [Protocol for deps erlang.mk - *deprecated*](doc_rules/elvis_project/protocol_for_deps_erlang_mk.md)
- [Protocol for deps rebar - *deprecated*](doc_rules/elvis_project/protocol_for_deps_rebar.md)
- [Protocol for deps](doc_rules/elvis_project/protocol_for_deps.md)

## Rulesets

Rulesets in `elvis` are used to group individual rules together and can save a lot of duplication.
`elvis` currently has five pre-defined rulesets, but gives you the ability to specify custom
rulesets in the configuration file.

The six pre-defined rulesets are:

- `elvis_config`, for elvis configuration files.
- `erl_files`, for Erlang source files (pre-defined rule set).
- `erl_files_strict`, for Erlang source files (all available rules).
- `gitignore`, for `.gitignore` files.
- `hrl_files`, for Erlang header files.
- `makefiles`, for Makefiles.
- `rebar_config`, for rebar configuration files.

Custom rulesets are defined in a `{<ruleset>, #{}}` tuple in `elvis`' configuration. Each key in the
map represents the ruleset name and is mapped to a list of rules as otherwise defined in a standard
ruleset.

Example configuration with a custom ruleset (named `my_ruleset`):

```erlang
[{elvis, [
    {rulesets,
        #{ my_ruleset => [{elvis_style, max_module_length, #{}}
                        , {elvis_style, no_common_caveats_call, #{}}
                         ]
         }
    }
  , {config,
        [#{ dirs => ["src/**" , "test/**"]
          , filter => "*.erl"
          , ruleset => my_ruleset
          }
        ]
    }
]}].
```

## The -elvis attribute

Per-module rules can also be configured using attribute `-elvis(_).`, with the same content as is
expected in `elvis.config`'s `rules` option, e.g.:

```erlang
-elvis([{elvis_style, no_behavior_info, #{}}]).
-elvis([{elvis_style, no_nested_try_catch}]).
```

**Note**: a single attribute with a list of rules is *the same* as multiple attributes with a list
of rules each - the rules are "merged" - as in:

```erlang
-elvis([{elvis_style, no_behavior_info, #{}}, {elvis_style, no_nested_try_catch}]).
```

In this case, the `ignore` attribute has limited value since it'll be ignored for "other" modules.
You can always play with the following, but results may not be surprising.

```erlang
-module(mymodule).
-elvis([{elvis_style, nesting_level, #{ level => 4, ignore => [mymodule] }}]).
...
```

## Disabling rules

Rules (as used by you, in `elvis.config`) come in 3-element tuples (if you use options) or 2-element
tuples (if you don't). To disable a rule, you need to use the 3-element form, and the reserved word
`disable`: let's consider you want to disable rule `elvis_text_style:no_tabs`; you do
`{elvis_text_style, no_tabs, disable}`, and you're done!

### The "ignore" option

Module-level rules implement a generic ignore mechanism that allows skipping analysis in elements of
your choice.
It suffices to add the `ignore` list to your rules, as per the example below.

```erlang
-elvis([{elvis_style, invalid_dynamic_call, #{ ignore => [elvis_core]}}]).
-elvis([{elvis_style, no_debug_call, #{ ignore => [elvis_result, elvis_utils]}}]).
```

You can add the exceptions using the following syntax:

- whole module: `ignore => [mod]`
- functions (by name only): `ignore => [{mod, fun}]` (available for **`elvis_style`-based rules
only**)
- module, function and arity: `ignore => [{mod, fun, arity}]` (available for **`elvis_style`-based
rules only**)

## BEAM files

Specific rules (signaled with "Works on `.beam` file? Yes!") allow you to perform analysis directly
on beam files (instead of source code).

Though this analysis may be useful for pin-pointing certain elements, beware that, e.g., reported
line numbers will most surely not correspond with those in the source file.

## Example `elvis.config`

```erlang
[{elvis, [
    {config, [
        #{ dirs => ["src/**", "test/**"]
         , filter => "*.erl"
         , ruleset => erl_files
         % these are not enforced by default, so are added here for completeness
         , rules => [{elvis_style, max_module_length, #{}}
                   , {elvis_style, no_common_caveats_call, #{}}
                    ]
         }
      , #{ dirs => ["include/**"]
         , filter => "*.hrl"
         , ruleset => hrl_files
         }
      , #{ dirs => ["."]
         , filter => "Makefile"
         , ruleset => makefiles
         , rules => [] }
      , #{ dirs => ["."]
         , filter => "rebar.config"
         , ruleset => rebar_config
         , rules => [] }
      , #{ dirs => ["."]
         , filter => "elvis.config"
         , ruleset => elvis_config
         , rules => [] }
      , #{ dirs => ["."]
         , filter => ".gitignore"
         , ruleset => gitignore }
    ]}
  , {verbose, true}
]}].
```
