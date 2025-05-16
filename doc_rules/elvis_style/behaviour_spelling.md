# Behaviour Spelling

Use a consistent spelling for behaviour attributes in modules.

> Works on `.beam` file? Yes!

## Problematic code

This is a convention for consistency, not a code problem.

Depending on your choice the problematic code can be

```erlang
-behavior(_). % the default "problematic"
```

or

```erlang
-behaviour(_).
```

## Correct code

Depending on your choice the correct code can be

```erlang
-behaviour(_). % the default "correct"
```

or

```erlang
-behavior(_).
```

## Rationale

By defining a particular spelling you increase consistency across your codebase.

Erlang's proposed default is (the one most found in the official documentation)
`behaviour`, but they also write "The spelling `behavior` is also accepted."

## Options

- `spelling :: behaviour | behavior`.
  - default: `behaviour`.

## Example

```erlang
{elvis_style, behaviour_spelling, #{ spelling => behaviour }}
```
