# Max Anonymous Function Clause Length [![](https://img.shields.io/badge/since-4.2.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.2.0)

The number of lines in an anonymous function clause definition should be limited to a defined maximum.

Lines containing only comments or whitespace may be either included or excluded from the line
count, depending on the configuration.

## Rationale

Limiting the number of lines in anonymous function clauses improves readability and maintainability.
Function clauses with too many lines tend to become more complex and harder to understand,
increasing the likelihood of introducing bugs. Keeping clauses concise encourages clear,
focused logic and makes it easier to navigate the codebase.

## Options

- `max_length :: non_neg_integer()`
  - default: `30`
- `count_comments :: boolean()`
  - default: `false`
- `count_whitespace :: boolean()`
  - default: `false`

## Example configuration

```erlang
{elvis_style, max_anonymous_function_clause_length, #{ max_length => 30
                                                     , count_comments => false
                                                     , count_whitespace => false
                                                     }}
```
