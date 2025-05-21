# Prefer unquoted atoms

(since [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0))

Atoms should not be quoted unless syntactically necessary.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Quick fix

Use an Erlang code formatter that enforces strict rules for quoted atoms.

## Options

- None.

## Example configuration

```erlang
{elvis_style, prefer_unquoted_atoms, #{}}
```
