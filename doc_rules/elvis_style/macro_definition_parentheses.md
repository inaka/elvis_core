# Macro Definition Parentheses [![](https://img.shields.io/badge/since-4.2.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.2.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Parentheses should be used propely to increase readability.
Macros that represent a function, should contains parentheses,
macros that represent a constant, should not.

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

Parentheses can help the reader to identify if there is a single value
hiding behind the macro key or if it's meant to replace some sort of code execution.

## Options

- None.

```erlang
{elvis_style, macro_definition_parentheses, #{}}
```
