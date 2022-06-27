# Max Module Length

This specifies an upper bound on module file **line** length. Lines that are comments and/or
whitespace can be either included or excluded from the line count.

**Notice**: this rule is not enforced by default. Check the
[example `elvis.config` file](#example-elvisconfig) below to see how you can enforce it.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected, since
the files are pre-processed)

## Options

- `max_length :: non_neg_integer()`.
  - default: `500`.
- `count_comments :: boolean()`
  - default: `false`
- `count_whitespace :: boolean()`
  - default: `false`

## Example

```erlang
{elvis_style, max_module_length}
%% or
{elvis_style, max_module_length, #{ max_length => 500
                                  , count_comments => false
                                  , count_whitespace => false
                                  }}
```
