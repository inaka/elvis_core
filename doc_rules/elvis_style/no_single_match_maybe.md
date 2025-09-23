# No Single-Match Maybe Blocks [![](https://img.shields.io/badge/since-4.1.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.1.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Single-match `maybe` blocks should be avoided.

> [!NOTE]  
> This rule only works under Erlang/OTP 27+.

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

Note that `maybe` blocks with an `else` are perfectly acceptable, too:

```erlang
maybe
    {ok, A} ?= do:something()
else
    OtherThing -> handle:this(OtherThing)
end
```

## Rationale

Using a `maybe` block with only one match is unnecessary and reduces code clarity. It adds
syntactic overhead without providing meaningful branching logic. In such cases, a let-style
assignment or direct pattern matching is typically more appropriate and idiomatic. Removing
single-match `maybe` block also improves readability and simplifies the control flow.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_single_match_maybe, #{}}
```
