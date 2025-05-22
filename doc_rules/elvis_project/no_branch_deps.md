# No Deps With Branches [![](https://img.shields.io/badge/since-2.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/2.0.0)

Dependencies in `rebar.config` should not use branches (e.g., `{branch, "my-branch"}`); use
`{tag, "..."}` or `{ref, "..."}` instead.

## Avoid

```erlang
{deps, [
    {my_dep, {git, "https://github.com/example/my_dep.git", {branch, "feature-xyz"}}}
]}.
```

## Prefer

```erlang
{deps, [
    {my_dep, {git, "https://github.com/example/my_dep.git", {tag, "1.2.3"}}}
]}.
```

## Rationale

Using a branch for dependencies makes builds non-reproducible because the commit pointed to by the
branch can change over time. This leads to inconsistencies between environments and makes debugging
more difficult. Commit references are immutable, ensuring that dependency versions remain
consistent across builds. While tags are not immutable they're most likely to not be moved than
branch heads.

## Options

- None.

## Example configuration

```erlang
{elvis_project, no_branch_deps, #{}}
```
