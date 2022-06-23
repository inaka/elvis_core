# Macro Module Names

Macros should not be used in dynamic calls, either in the module position
(i.e. `?SOME_MACRO:function_name()`) or the function position (i.e. `module_name:?FUNCTION()`). An
exception to this is the usage of the `?MODULE` macro in the module position.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Options

- None.

## Example

```erlang
{elvis_style, macro_module_names}
%% or
{elvis_style, macro_module_names, #{}}
```
