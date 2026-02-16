# Consistent Variable Naming ![](https://img.shields.io/badge/BEAM-yes-orange)

Variables that represent the same concept should be named consistently across a module.
This rule detects syntax or casing differences: the same logical name written in different
styles, such as `TypeVar` vs `Type_var` or `MyVar` vs `My_Var`.

## Avoid

> This is a convention aimed at preventing subtle bugs and improving readability.

Mixing naming styles for the same variable across functions or type definitions:

```erlang
-type one(TypeVar) :: {one, TypeVar}.
-type two(Type_var) :: {two, Type_var}.
```

## Prefer

Pick one style per variable name and use it everywhere in the module:

```erlang
-type one(TypeVar) :: {one, TypeVar}.
-type two(TypeVar) :: {two, TypeVar}.
```

## Rationale

Inconsistent variable naming makes code harder to read and search. Casing mismatches
like `TypeVar` vs `Type_var` can confuse readers about whether two different variables
are intended.

## Options

- None.

## Example configuration

```erlang
{elvis_style, consistent_variable_naming, #{}}
```
