# No throw

(since [1.4.0](https://github.com/inaka/elvis_core/releases/tag/1.4.0))

Raising exceptions with `throw` should be avoided.

> Works on `.beam` file? Yes!

## Avoid

Especially when not used for non-local returns from functions (e.g. early return).

```erlang
throw({error, "Something went wrong!"}).
```

## Prefer

The preferred way to raise an exception is by using `error/1`:

```erlang
error({error, "Something went wrong!"}).
```

## Rationale

`throw/1` is intended for module-local flow control and non-local returns from functions, not for
raising exceptions. While valid, it differs from `error/1` and `exit/1`, which are designed for
proper exception handling. Using `throw/1` can lead to unpredictable behavior, as it is caught as
a non-local return, whereas `error/1` and `exit/1` provide better control and error context
(such as stack traces).

## Options

- None.

## Example

```erlang
{elvis_style, no_throw, #{}}
```
