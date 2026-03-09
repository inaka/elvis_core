# Protocol For Deps `erlang.mk` [![](https://img.shields.io/badge/until-1.4.0-red)](https://github.com/inaka/elvis_core/releases/tag/1.4.0)

`erlang.mk` dependencies should use known protocols.

> [!WARNING]
> This rule was removed in version [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0).

## Options

- `regex :: string()`
  - default: `(https://.*|[0-9]+([.][0-9]+)*)`

## Example configuration

```erlang
{elvis_project, protocol_for_deps_erlang_mk, #{ regex => "(https://.*|[0-9]+([.][0-9]+)*)" }}
```
