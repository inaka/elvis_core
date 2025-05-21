# Consistent Generic Type

(since [2.0.1](https://github.com/inaka/elvis_core/releases/tag/2.0.1))

`term()` or `any()` should be used consistently in typespecs.

> Works on `.beam` file? Yes!

## Avoid

> This is a convention aimed at ensuring consistency, rather than a coding issue.

Depending on your choice, you should avoid:

```erlang
-type mytype() :: any(). % the default "avoid"
```

or:

```erlang
-type mytype() :: term().
```

## Prefer

Depending on your choice, you should prefer:

```erlang
-type mytype() :: term(). % the default "prefer"
```

or:

```erlang
-type mytype() :: any().
```

## Rationale

Defining a specific "term" type enhances consistency across your codebase.

## Options

- `preferred_type :: term | any`
  - default: `term`

## Example configuration

```erlang
{elvis_style, consistent_generic_type, #{ preferred_type => term }}
```
