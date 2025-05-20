# Invalid Dynamic Calls

Calls to non-local and non-external functions, in modules that do not include a `callback`
attribute, should be avoided.

> Works on `.beam` file? Yes!

## Rationale

This is a relatively complex topic, which is thoroughly covered in
[Erlang Battleground - Erlang Behaviors](https://medium.com/erlang-battleground/erlang-behaviors-4348e89351ff).

## Options

- None.

## Example

```erlang
{elvis_style, invalid_dynamic_call, #{}}
```
