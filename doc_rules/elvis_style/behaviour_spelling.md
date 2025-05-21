# Behaviour Spelling [![](https://img.shields.io/badge/since-1.3.0-blue)](https://github.com/inaka/elvis_core/releases/tag/1.3.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Behavior attribute names in modules should follow a consistent spelling convention.

## Avoid

> This is a convention aimed at ensuring consistency, rather than a coding issue.

Depending on your choice, you should avoid:

```erlang
-behavior(_). % the default "avoid"
```

or:

```erlang
-behaviour(_).
```

## Prefer

Depending on your choice, you should prefer:

```erlang
-behaviour(_). % the default "prefer"
```

or:

```erlang
-behavior(_).
```

## Rationale

Defining a consistent spelling improves uniformity across your codebase.

Erlang/OTP's proposed default spelling is `behaviour`, as seen most frequently in the official
documentation. However, the spelling `behavior` is also accepted.

## Options

- `spelling :: behaviour | behavior`
  - default: `behaviour`

## Example configuration

```erlang
{elvis_style, behaviour_spelling, #{ spelling => behaviour }}
```
