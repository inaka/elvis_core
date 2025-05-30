# Parentheses in Macro Definitions

Parentheses should be used propely to increase readability.
Macros that represents a function, should contains parentheses,
macros that represents a constan, shouldn't.

## Avoid
```
-define(NOT_CONSTANT, application:get_env(myapp, key)).
-define(CONSTANT(), 100).
```

## Prefer
```
-define(NOT_CONSTANT(), application:get_env(myapp, key)).
-define(CONSTANT, 100).
```

## Rationale

Parentheses can help the reader to identify the value hiding behind the macro key.

## Options

- None.

```erlang
{elvis_style, parantheses_in_macro_defs, #{}}
```
