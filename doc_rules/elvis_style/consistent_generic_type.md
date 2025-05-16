# Consistent Generic Type

Use `term()` or `any()` consistently for types in specs.

> Works on `.beam` file? Yes!

## Problematic code

This is a convention for consistency, not a code problem.

Depending on your choice the problematic code can be

```erlang
-type mytype :: any(). % the default "problematic"
```

or

```erlang
-type mytype :: term().
```

## Correct code

Depending on your choice the correct code can be

```erlang
-type mytype :: term(). % the default "correct"
```

or

```erlang
-type mytype :: any().
```

## Rationale

By defining a particular "term" type you increase consistency across your code base.

## Options

- `preferred_type :: term | any`.
  - default: `term`.

## Example

```erlang
{elvis_style, consistent_generic_type, #{ preferred_type => term }}
```
