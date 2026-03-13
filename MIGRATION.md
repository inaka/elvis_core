# Migration guide

This file presents the changes required to your consumption code/configuration when you bump
from a given version to a new one, as presented below.
If you're bumping more than a single version, be sure to respect all the intermediate instructions,
since these are incremental.

This file's format is influenced by [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and
[Make a README](https://www.makeareadme.com/).

## Going from `4.x` to `5.0.0`

### Avoiding warnings on the use of `erlang:garbage_collect/0`

The `no_common_caveats_call` rule now includes `erlang:garbage_collect/0` in its default set of
restricted functions.

If you need to permit this call in your codebase, you have two primary options:

- **fully disable the rule**: disable `no_common_caveats_call` entirely in your configuration.

- **customize the list**: modify the `caveat_functions` option to define a specific subset of
functions that better fits your project's requirements.

### Drop the `elvis` wrapper from your configuration

**Before (4.x):**

```erlang
% elvis.config
[
  {elvis, [
    {config, [...]},
    ...
  ]}
].
```

**After (5.0.0):**

```erlang
% elvis.config
[
  {config, [...]},
  ...
].
```

### Using the new interface: `elvis_core:rock/1,2,3`

Functions `elvis_core:rock/1,2,3` have been updated to return `ok | {errors, _} | {warnings, _}`.

The tuple label (`errors` or `warnings`) serves as a signal for the exit status code
(use `0` for `warnings`, and non-`0` for `errors`).

**Future compatibility**: we are intentionally treating the second element of the tuple as opaque.
This allows us to refine the return type - likely replacing the placeholder with structured
diagnostic data - once `elvis_core` implements a decoupled output framework (e.g., removing
direct `io:format/2` calls).

### Using `elvis_core:rock/1` instead of `_:rock_this/2`

The function `_:rock_this/2` has been removed. Use the existing `elvis_core:rock/1`
function, which utilizes a configuration list.

**Before (4.x):**

You could pass a single file or a module name directly:

```erlang
% As a filename
elvis_core:rock_this("rebar.config", ElvisConfig).

% As a module name
elvis_core:rock_this(elvis_core, ElvisConfig).
```

**After (5.0.0):**

You must now provide a list of configurations, with `files` and `ruleset`:

```erlang
% As a filename
elvis_core:rock({config, [#{
    files => ["rebar.config"],
    ruleset => rebar_config
}]}).

% As a module (requires the explicit file path)
elvis_core:rock({config, [#{
    files => ["src/elvis_core.erl"],
    ruleset => erl_files
}]}).
```

**Note**: use `elvis_config:config/0` to retrieve the configuration for the current project, if
required during the migration.

### Replace `dirs` and `filter` with `files`

Config no longer uses `dirs` and `filter`. Use a single `files` key: a list of glob patterns that
select which files each rule group applies to.

**Before (4.x):**

```erlang
#{ dirs => ["src/**", "test/**"]
 , filter => "*.erl"
 , ruleset => erl_files
 , rules => [...]
}
```

**After (5.0.0):**

```erlang
#{ files => ["src/**/*.erl", "test/**/*.erl"]
 , ruleset => erl_files
 , rules => [...]
}
```

For each entry in `dirs`, combine it with `filter` into one glob: e.g. `"src/**"` and `"*.erl"`
become `"src/**/*.erl"`. For a single dir and filter, e.g. `dirs => ["."]` and `filter =>
"rebar.config"`, use `files => ["rebar.config"]` (or `["**/rebar.config"]` if you need to match in
subdirs).

**Example — full config before (4.x):**

```erlang
#{ dirs => ["src/**"], filter => "*.erl", ruleset => erl_files, rules => [...] }
, #{ dirs => ["include/**"], filter => "*.hrl", ruleset => hrl_files }
, #{ dirs => ["."], filter => "rebar.config", ruleset => rebar_config, rules => [] }
, #{ dirs => ["."], filter => ".gitignore", ruleset => gitignore }
```

**After (5.0.0):**

```erlang
#{ files => ["src/**/*.erl"], ruleset => erl_files, rules => [...] }
, #{ files => ["include/**/*.hrl"], ruleset => hrl_files }
, #{ files => ["rebar.config"], ruleset => rebar_config, rules => [] }
, #{ files => [".gitignore"], ruleset => gitignore }
```

Allowed config keys per rule group are now: `files`, `ignore`, `ruleset`, `rules`. At least one glob
in `files` must match at least one file.

### Rule renames

If your `rules` override or reference these rule names, update them to the new names:

| Old name | New name |
|----------|----------|
| `god_modules` | `no_god_modules` |
| `nesting_level` | `no_deep_nesting` |
| `invalid_dynamic_call` | `no_invalid_dynamic_calls` |
| `used_ignored_variable` | `no_used_ignored_variables` |
| `macro_names` | `macro_naming_convention` |
| `consistent_generic_type` | `generic_type` |
| `no_nested_hrls` | `no_includes` |

Example: `{elvis_style, no_nested_hrls, #{}}` → `{elvis_style, no_includes, #{}}`.

Using an old name prints a warning and the rule is skipped; update your config to the new name to
run the rule.

### Rule namespace change

- **`prefer_unquoted_atoms`** moved from `elvis_text_style` to `elvis_style` (before 4.2.0). Use
  `{elvis_style, prefer_unquoted_atoms, ...}` instead of `{elvis_text_style, prefer_unquoted_atoms, ...}`.

### Removed rules

These rules no longer exist. Remove them from your config or switch to the replacement:

| Removed rule | Replacement or note |
|--------------|----------------------|
| `elvis_style:macro_module_names` | No replacement. |
| `elvis_project:no_deps_master_erlang_mk` | Use `no_branch_deps`. |
| `elvis_project:no_deps_master_rebar` | Use `no_branch_deps`. |
| `elvis_project:old_configuration_format` | No replacement. |
| `elvis_project:protocol_for_deps_rebar` | Use `protocol_for_deps`. |
| `elvis_project:protocol_for_deps_erlang_mk` | No replacement. |

Referenced removed rules are skipped with a warning.

### Stricter configuration validation

Configuration is validated more strictly: only the keys `files`, `ignore`, `ruleset`, and `rules` are
allowed per rule group; unknown keys cause validation to fail. Non‑existing rules or rulesets also
fail validation (renamed/removed rules are handled as above and do not fail validation).

### Parallelism default

The default for the `parallel` application option (number of parallel workers) is now
`erlang:system_info(schedulers_online)` instead of the previous fixed value. If you relied on the
old default, set `application:set_env(elvis_core, parallel, N)` explicitly.

## Going from `3.x` to `4.x`

### Update

- your `atom_naming_convention`'s options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/atom_naming_convention.md#options>
- your `macro_names`' options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/macro_names.md#options>
- your `operator_spaces` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/operator_spaces.md#options>
- your `no_space` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/no_space.md#options>
- your `function_naming_convention` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/function_naming_convention.md#options>
- your `module_naming_convention` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/module_naming_convention.md#options>
- your `no_debug_call` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/no_debug_call.md#options>
- your `no_common_caveats_call` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/no_common_caveats_call.md#options>

On the other hand you may choose to not implement those changes and merely adapt your code.

## Going from `0.x` to `1.x`

### Update

- your `ignore` option, from rule `max_function_length`, to also contain the content of
`ignore_functions`
- your rule configuration files (e.g. `elvis.config`), by moving `elvis_style` to
`elvis_text_style` for the following rules: `line_length`, `no_tabs`, and `no_trailing_whitespace`

### Delete

- option `ignore_functions`, from rule `max_function_length`
