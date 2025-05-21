# No Single-Match Maybe Statements [![](https://img.shields.io/badge/since-4.1.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.1.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Single-match maybe statements should be avoided.

## Avoid

```erlang
maybe
    {ok, A} ?= do:something()
end
```

## Prefer

```erlang
{ok, A} = do:something()
```

Note that `maybe` statements with an `else` are perfectly acceptable, too:

```erlang
maybe
    {ok, A} ?= do:something()
else
    OtherThing -> handle:this(OtherThing)
end
```

## Rationale

Using a `maybe` expression with only one match is unnecessary and reduces code clarity. It adds
syntactic overhead without providing meaningful branching logic. In such cases, a let-style
assignment or direct pattern matching is typically more appropriate and idiomatic. Removing
single-match `maybe` statements also improves readability and simplifies the control flow.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_single_match_maybe, #{}}
```
