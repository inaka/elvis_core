# No deps with branches

(since [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0))

Don't use branches (i.e. `{branch, "my-branch"}`) for deps in `rebar.config` files.
Prefer `tag` or `ref` instead.

## Config

- None.

## Example

```erlang
{elvis_project, no_branch_deps, #{}}
```
