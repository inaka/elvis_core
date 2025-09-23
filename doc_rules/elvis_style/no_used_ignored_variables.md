# No Used Ignored Variables ![](https://img.shields.io/badge/BEAM-yes-orange)

> [!NOTE]
> This rule was named `used_ignored_variable` before [4.2.0](https://github.com/inaka/elvis_core/releases/tag/4.2.0).

Declared anonymous variables (prefixed with `_`) should not be used.

## Avoid

```erlang
_Ignored = some_function(),
do_something(_Ignored).
```

## Prefer

```erlang
Value = some_function(),
do_something(Value).
```

## Rationale

In Erlang, variables prefixed with an underscore (e.g., `_Var`) are conventionally treated as
"ignored" or "don't care" values, signaling that they are unused. However, using these variables in
later expressions (e.g., passing `_Var` to a function or matching against it) contradicts their
intended purpose and misleads both the compiler and human readers.

This can confuse maintainers, as `_Var` appears to be intentionally ignored, and also introduce
subtle bugs or overlooked logic due to mistaken assumptions about variable usage.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_used_ignored_variables, #{}}
```
