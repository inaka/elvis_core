# No Deps `master` `rebar.config` [![](https://img.shields.io/badge/until-1.4.0-red)](https://github.com/inaka/elvis_core/releases/tag/1.4.0)

Dependencies in `rebar.config` should not use branch `master` (e.g., `{branch, "master"}`); use
`{tag, "..."}` or `{ref, "..."}` instead.

> #### Warning {: .warning}
>
> This rule was removed in version [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0);
> use `no_branch_deps` instead.

## Options

- None.

## Example configuration

```erlang
{elvis_project, no_deps_master_rebar, #{}}
```
