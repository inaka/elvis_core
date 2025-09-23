# Max Anonymous Function Length

The number of lines in an anonymous function definition should be limited to a defined maximum.

Lines containing only comments or whitespace may be either included or excluded from the line
count, depending on the configuration.

## Rationale

Limiting the number of lines in anonymous function definitions improves readability and maintainability.
Functions with too many lines tend to become more complex and harder to understand,
increasing the likelihood of introducing bugs. Keeping functions concise encourages clear,
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
{elvis_style, max_anonymous_function_length, #{ max_length => 30
                                              , count_comments => false
                                              , count_whitespace => false
                                              }}
```
