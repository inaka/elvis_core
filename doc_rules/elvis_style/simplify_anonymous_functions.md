# Simplify Anonymous Functions [![](https://img.shields.io/badge/since-4.2.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.2.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Anonymous functions that simply call a named function (with the same arguments in the same order) should be avoided; they can be more concisely expressed using the function reference syntax.

## Avoid

```erlang
fun(Pattern) -> is_match_node(Pattern) end
```

## Prefer

```erlang
fun is_match_node/1
```

## Rationale

The `fun F/A` syntax is clearer and more concise when the anonymous function does nothing more than
call another function. It reduces noise and makes the code easier to read and maintain by removing
unnecessary bindings and boilerplate.

## Options

- None.

## Example configuration

```erlang
{elvis_style, simplify_anonymous_functions, #{}}
```
