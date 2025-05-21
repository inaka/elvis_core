# Consistent Variable Casing

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Variables with the same name should follow a consistent casing style (either upper or lower case)
to avoid mismatches, such as `UserID` being mixed with `UserId` or `Userid`.

> Works on `.beam` file? Yes!

## Avoid

> This is a convention aimed at ensuring consistency, rather than a coding issue.

Depending on your choice, you should avoid:

```erlang
UserID = 123,
...
```

in one function, followed by

```erlang
UserId = 123,
...
```

in another one.

## Prefer

Adhere to a single casing style and apply it wherever Elvis identifies an inconsistency.

## Rationale

Defining a consistent casing style enhances uniformity across your codebase and facilitates
case-sensitive, whole-word searches.

## Options

- None.

## Example configuration

```erlang
{elvis_style, consistent_variable_casing, #{}}
```
