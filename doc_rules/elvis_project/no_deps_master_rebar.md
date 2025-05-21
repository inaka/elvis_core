# No deps master rebar

(not available since [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0))

Dependencies in `rebar.config` should not use branch `master` (e.g., `{branch, "master"}`); use
`{tag, "..."}` or `{ref, "..."}` instead.

> [!WARNING]
> This rule is now deprecated, having been replaced by `no_branch_deps`.

## Options

- None.

## Example

```erlang
{elvis_project, no_deps_master_rebar, #{}}
```
