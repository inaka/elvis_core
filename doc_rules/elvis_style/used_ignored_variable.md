# Used Ignored Variable

Don't use a variable whose name indicates it is actually an ignored variable (i.e., it starts with
an underscore, like `_Var`).

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, used_ignored_variable}
%% or
{elvis_style, used_ignored_variable, #{}}
```
