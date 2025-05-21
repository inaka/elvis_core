# No Nested try...catch Blocks

(since [0.4.0](https://github.com/inaka/elvis_core/releases/tag/0.4.0))

`try...catch` expressions should not be nested.

> Works on `.beam` file? Yes!

## Avoid

```erlang
    try
        {ok, KeepGoing} = do:something(),
        try
            do:something_else("and", KeepGoing)
        catch
            _:_ -> {ignore, errors, here}
        end
    catch
        Kind:Error ->
            try this:block(is, also, not_nested) catch _:_ -> {Kind, Error} end
    after
        try this:one(is, "not", nested, either) catch _:_ -> ok end
    end.
```

## Prefer

```erlang
    try do:something() of
        {ok, KeepGoing} ->
            try
                do:something_else("and", KeepGoing)
            catch
                _:_ -> {ignore, errors, here}
            end
    catch
        Kind:Error ->
            try this:block(is, also, not_nested) catch _:_ -> {Kind, Error} end
    after
        try this:one(is, "not", nested, either) catch _:_ -> ok end
    end.
```

## Rationale

Nesting `try...catch` expressions makes code harder to read, reason about, and maintain. It
obscures the control flow and complicates exception handling, making it more difficult to determine
which block handles which error. Instead, error handling should be flattened and separated into
distinct functions or simplified structures where possible. This leads to more readable and robust
code with clearer error semantics.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_nested_try_catch, #{}}
```
