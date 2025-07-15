# Prefer Unquoted Atoms [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0)

> [!NOTE]
> This rule belonged to the `elvis_text_style` namespace before [4.2.0](https://github.com/inaka/elvis_core/releases/tag/4.2.0).

Atoms should not be quoted unless syntactically necessary.

## Quick fix

Use an Erlang code formatter that enforces strict rules for quoted atoms.

## Options

- None.

## Example configuration

```erlang
{elvis_style, prefer_unquoted_atoms, #{}}
```
