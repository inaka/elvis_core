# Protocol for deps erlang.mk

(not available since [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0))

`erlang.mk` dependencies should use known protocols.

> [!WARNING]
> This rule is now deprecated, as is the support for `erlang.mk`.

## Options

- `regex :: string()`
  - default: `(https://.*|[0-9]+([.][0-9]+)*)`

## Example

```erlang
{elvis_project, protocol_for_deps_erlang_mk, #{ regex => "(https://.*|[0-9]+([.][0-9]+)*)" }}
```
