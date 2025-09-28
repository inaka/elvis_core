# Macro Definition Parentheses

Parentheses should be used propely to increase readability.
Macros that represent a function, should contains parentheses,
macros that represent a constan, should not.

## Avoid

```erlang
-define(NOT_CONSTANT, application:get_env(myapp, key)).
-define(CONSTANT(), 100).
```

## Prefer

```erlang
-define(NOT_CONSTANT(), application:get_env(myapp, key)).
-define(CONSTANT, 100).
```

## Rationale

Parentheses can help the reader to identify if there is a single value hiding behind the macro key or if it's meant to replace some sort of code execution.

## Options

- None.

```erlang
{elvis_style, macro_definition_parentheses, #{}}
```
