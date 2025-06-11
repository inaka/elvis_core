# Invalid Dynamic Calls ![](https://img.shields.io/badge/BEAM-yes-orange)

Calls to non-local and non-external functions, in modules that do not include a `callback`
attribute, should be avoided. This includes:

* Calls with a variable in the module position (e.g., `Module:call()`).
* Calls with a variable in the function position (e.g., `module:Function()`).
* Calls with a macro in the module position (e.g., `?A_MODULE:call()`).
  * Except when the macro is `?MDOULE`, because this is a common pattern used for hot-code reloading.
* Calls with a macro in the function position (e.g., `module:?FUNCTION_NAME()`).
* Calls with a function call in the module position (e.g., `(get:the_module()):call()`).
* Calls with a function call in the function position (e.g., `module:(get:the_function())()`).

## Rationale

The use of dynamic function calls hinders (and sometimes outright prevents) static code analyzers
like Xref, Dialyzer, and Hank from checking the code. At the same time, it makes the code obscure
for developers reading and debugging it.
The reason why this is allowed for callback-defining modules is a relatively complex topic,
which is thoroughly covered in
[Erlang Battleground - Erlang Behaviors](https://medium.com/erlang-battleground/erlang-behaviors-4348e89351ff).

## Options

- None.

## Example configuration

```erlang
{elvis_style, invalid_dynamic_call, #{}}
```
