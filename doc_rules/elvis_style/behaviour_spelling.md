# Behaviour Spelling

Behavior attribute names in modules should follow a consistent spelling convention.

> Works on `.beam` file? Yes!

## Problematic code

> This is a convention aimed at ensuring consistency, rather than a coding issue.

Depending on your choice, the problematic code may be:

```erlang
-behavior(_). % the default "problematic"
```

or (depending on your preference)

```erlang
-behaviour(_).
```

## Correct code

Depending on your choice, the correct code may be:

```erlang
-behaviour(_). % the default "correct"
```

or (depending on your preference)

```erlang
-behavior(_).
```

## Rationale

Defining a consistent spelling improves uniformity across your codebase.

Erlang's proposed default spelling is `behaviour`, as seen most frequently in the official
documentation. However, the spelling `behavior` is also accepted.

## Options

- `spelling :: behaviour | behavior`
  - default: `behaviour`

## Example

```erlang
{elvis_style, behaviour_spelling, #{ spelling => behaviour }}
```
