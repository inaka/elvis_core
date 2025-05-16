# Consistent Variable Casing

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

All variables with the _same name_ should use the same (upper or lower) casing style.
This is to avoid instances of `UserID` mixed with instances of `UserId` or `Userid`.

## Problematic code

This is a convention for consistency, not a code problem.

Depending on your choice the problematic code can be, for example

```erlang
  UserID = 123,
  ...
```

in one function, followed by

```erlang
  UserId = 124,
  ...
```

in another one.

## Correct code

Stick to one casing style and apply it where Elvis told you it found an
inconsistency.

## Rationale

By defining a particular casing style you increase consistency across your code base,
and it also makes it easier to perform case-sensitive, whole-word searches.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, consistent_variable_casing}
```
