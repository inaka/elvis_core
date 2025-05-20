# Max Function Clause Length

(since [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0))

The number of lines in a function clause definition should be limited to a defined maximum.

Lines containing only comments or whitespace may be either included or excluded from the line
count, depending on the configuration

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected, since
the files are pre-processed)

## Rationale

Limiting the number of lines in function clauses improves readability and maintainability.
Function clauses with too many lines tend to become more complex and harder to understand,
increasing the likelihood of introducing bugs. Keeping function clauses concise encourages clear,
focused logic and makes it easier to navigate the codebase.

## Options

- `max_length :: non_neg_integer()`
  - default: `30`
- `count_comments :: boolean()`
  - default: `false`
- `count_whitespace :: boolean()`
  - default: `false`

## Example

```erlang
{elvis_style, max_function_clause_length, #{ max_length => 30
                                           , count_comments => false
                                           , count_whitespace => false
                                           }}
```
