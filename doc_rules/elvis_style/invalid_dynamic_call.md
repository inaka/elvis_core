# Invalid Dynamic Calls ![](https://img.shields.io/badge/BEAM-yes-orange)

Calls to non-local and non-external functions, in modules that do not include a `callback`
attribute, should be avoided.

## Rationale

This is a relatively complex topic, which is thoroughly covered in
[Erlang Battleground - Erlang Behaviors](https://medium.com/erlang-battleground/erlang-behaviors-4348e89351ff).

## Options

- None.

## Example configuration

```erlang
{elvis_style, invalid_dynamic_call, #{}}
```
