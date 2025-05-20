# No Single-Clause Maybe Statements

(since [4.1.0](https://github.com/inaka/elvis_core/releases/tag/4.1.0))

Don't write code like this:

```erlang
maybe
    {ok, A} ?= do:something()
end
```

Use pattern-matching instead:

```erlang
{ok, A} = do:something()
```

Note that this is valid code, tho:

```erlang
maybe
    {ok, A} ?= do:something()
else
    OtherThing -> handle:this(OtherThing)
end
```

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_single_match_maybe, #{}}
```
