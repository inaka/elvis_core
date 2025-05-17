# Macro Module Names

Avoid using macros in dynamic function calls.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Exceptions

The use of `?MODULE` is permitted in the form `?MODULE:func()`.

## Problematic code

```erlang
?MYMOD:myfunc()

mymod:?MYFUNC()
```

## Correct code

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

## Example

```erlang
{elvis_style, macro_module_names, #{}}
```
