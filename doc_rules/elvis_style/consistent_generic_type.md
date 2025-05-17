# Consistent Generic Type

Use `term()` or `any()` consistently when specifying types in specs.

> Works on `.beam` file? Yes!

## Problematic code

This is a convention aimed at ensuring consistency, rather than a coding issue.

Depending on your choice, the problematic code may be:

```erlang
-type mytype :: any(). % the default "problematic"
```

or

```erlang
-type mytype :: term().
```

## Correct code

Depending on your choice, the correct code may be:

```erlang
-type mytype :: term(). % the default "correct"
```

or

```erlang
-type mytype :: any().
```

## Rationale

Defining a specific "term" type enhances consistency across your codebase.

## Options

- `preferred_type :: term | any`.
  - default: `term`.

## Example

```erlang
{elvis_style, consistent_generic_type, #{ preferred_type => term }}
```
