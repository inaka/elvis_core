# No Catch Expressions [![](https://img.shields.io/badge/since-1.4.0-blue)](https://github.com/inaka/elvis_core/releases/tag/1.4.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Avoid use of `catch` expressions.

## Avoid

```erlang
risky_call() ->
    catch do_something().
```

## Prefer

```erlang
risky_call() ->
    try do_something() of
        Result -> Result
    catch
        Class:Reason ->
            handle_error(Class, Reason)
    end.
```

## Rationale

The `catch` expression in Erlang catches all kinds of exceptions - including runtime errors,
`throw`, and `exit` signals - without distinction. This can obscure the source and type of errors,
making debugging and reasoning about failure states more difficult. In contrast, `try ... catch`
allows finer-grained control over different types of exceptions and supports pattern matching on
the reason and stack trace, enabling safer and more maintainable error handling.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_catch_expressions, #{}}
```
