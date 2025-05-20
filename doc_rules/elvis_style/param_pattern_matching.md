# Param Pattern-Matching

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Function argument capture pattern-matching should be consistently defined in the code.

> Works on `.beam` file? Yes!

## Avoid

> This is a convention aimed at ensuring consistency, rather than a coding issue.

Depending on your choice, you should avoid:

```erlang
myfunc(Params = #{pattern := ToMatch}) % the default "avoid"
```

or:

```erlang
myfunc(#{pattern := ToMatch} = Params)
```

## Prefer

Depending on your choice, you should prefer:

```erlang
myfunc(#{pattern := ToMatch} = Params) % the default "prefer"
```

or:

```erlang
myfunc(Params = #{pattern := ToMatch})
```

## Rationale

When capturing arguments through pattern matching in function clauses, it’s important to maintain
consistency across the codebase. Capturing function arguments with different styles (e.g., placing
the variable on the left or right side of the pattern) can lead to confusion and make the code
harder to read. Consistency in style improves maintainability, reduces the likelihood of errors,
and makes the code easier to understand for all developers. This rule ensures that the style of
pattern matching for function arguments is uniform throughout the codebase.

## Options

- `side :: left | right`
  - default: `right`

## Example

```erlang
{elvis_style, param_pattern_matching, #{ side => right }}
```
