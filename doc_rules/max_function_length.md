# Max Function Length

This specifies an upper bound on function **line** length. Lines that are comments and/or whitespace
can be either included or excluded from the line count.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected, since
the files are pre-processed)

## Options

- `max_length :: non_neg_integer()`.
  - default: `30`.
- `count_comments :: boolean()`
  - default: `false`
- `count_whitespace :: boolean()`
  - default: `false`

## Example

```erlang
{elvis_style, max_function_length}
%% or
{elvis_style, max_function_length, #{ max_length => 30
                                    , count_comments => false
                                    , count_whitespace => false
                                    }}
```
