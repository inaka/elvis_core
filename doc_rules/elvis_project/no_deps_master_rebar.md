# No deps master rebar [![](https://img.shields.io/badge/until-1.4.0-red)](https://github.com/inaka/elvis_core/releases/tag/1.4.0)

Dependencies in `rebar.config` should not use branch `master` (e.g., `{branch, "master"}`); use
`{tag, "..."}` or `{ref, "..."}` instead.

> [!WARNING]
> This rule is now deprecated, having been replaced by `no_branch_deps`.

## Options

- None.

## Example configuration

```erlang
{elvis_project, no_deps_master_rebar, #{}}
```
