# Macro Module Names

The use of macros in dynamic function calls should be avoided.

> [!WARNING]
> This rule is deprecated since [4.1.0](https://github.com/inaka/elvis_core/releases/tag/4.1.0).

## Exceptions

The use of `?MODULE` is permitted in the form `?MODULE:func()` because it's the common idiom for
functions that should always pick up the latest version of the module when doing hot-code reloading.

## Avoid

```erlang
?MYMOD:myfunc()

mymod:?MYFUNC()
```

## Prefer

```erlang
mymod:myfunc()
```

## Rationale

Using macros in dynamic function calls can obscure the intent of the code and hinder readability,
static analysis, and refactoring tools. Macros expand at compile time and may introduce complexity
or unexpected behavior when used as dynamic module or function references, making the code harder
to reason about and debug.

## Options

- None.

## Example configuration

```erlang
{elvis_style, macro_module_names, #{}}
```
