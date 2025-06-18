# God Modules ![](https://img.shields.io/badge/BEAM-yes-orange)

The number of exported functions in a module should be limited to a maximum threshold.

## Rationale

Limiting the number of functions exported from a module helps maintain clarity, modularity, and
readability in the codebase. A module with too many exported functions can become difficult to
consume and understand, from an API perspective. By restricting the number of exported functions,
you encourage smaller, more focused modules that adhere to the [Single Responsibility Principle](https://en.wikipedia.org/wiki/Single-responsibility_principle)
(SRP). This also makes the code easier to test, debug, and extend.

## Options

- `limit :: non_neg_integer()`
  - default: `25`

## Example configuration

```erlang
{elvis_style, god_modules, #{ limit => 25 }}
```
