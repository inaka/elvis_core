# No Single-Clause Case Statements

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Don't write code like this:

```erlang
case do:something() of
    {ok, Result} -> do:something("else")
end
```

Use pattern-matching instead:

```erlang
{ok, Result} = do:something(),
do:something("else")
```

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_single_clause_case, #{}}
```
