# Max Module Length

The number of lines in a module should be limited to a defined maximum.

Lines containing only comments or whitespace may be either included or excluded from the line
count, depending on the configuration

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected, since
the files are pre-processed)

## Rationale

Limiting the number of lines in a module improves readability and maintainability.
Modules with too many lines tend to become more complex and harder to understand,
increasing the likelihood of introducing bugs. Keeping modules concise encourages clear,
focused logic and makes it easier to navigate the codebase.

## Options

- `max_length :: non_neg_integer()`
  - default: `500`
- `count_comments :: boolean()`
  - default: `false`
- `count_whitespace :: boolean()`
  - default: `false`
- `count_docs :: boolean()`
  - default: `false`

## Example

```erlang
{elvis_style, max_module_length, #{ max_length => 500
                                  , count_comments => false
                                  , count_whitespace => false
                                  , count_docs => false
                                  }}
```
