# Consistent Variable Casing

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

All variables with the _same name_ should use the same (upper or lower) casing style.
This is to avoid instances of `UserID` mixed with instances of `UserId` or `Userid`.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, consistent_variable_casing}
```
