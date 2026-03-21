# Prefer Unquoted Atoms [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0)

> #### Note {: .info}
>
> This rule belonged to the `elvis_text_style` namespace before [4.2.0](https://github.com/inaka/elvis_core/releases/tag/4.2.0).

Atoms should not be quoted unless syntactically necessary.

## Avoid

```erlang
case Value of
    'ok' -> done
end.
```

## Prefer

```erlang
case Value of
    ok -> done
end.
```

When the atom letters are lowercase and the atom is not a reserved word, quotes add noise without
helping the parser.

## Rationale

Unquoted atoms are the usual Erlang style when the language allows them. Removing unnecessary
`'`...`'` improves readability and matches what readers expect. Atoms that **must** stay
quoted—reserved words like `maybe`, uppercase names, or atoms with special characters—are **not**
reported by this rule.

## Quick fix

Use an Erlang code formatter that enforces strict rules for quoted atoms.

## Options

- None.

## Example configuration

```erlang
{elvis_style, prefer_unquoted_atoms, #{}}
```
