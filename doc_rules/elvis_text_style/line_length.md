# Line Length

No line should be longer than a given limit. Comments can be skipped.

> Works on `.beam` file? No.

## Options

- `limit :: pos_integer()`
  - default: `100`
- `skip_comments :: false | any | whole_line`
  - default: `false`, means _emit a warning for every line that goes over `Limit`_
  - `any` means _don't emit a warning if the part of the line that goes over `Limit` belongs to a
  comment_
  - `whole_line` means _don't emit a warning if the line that goes over `Limit` is __just__ a comment_
- `no_whitespace_after_limit :: boolean()`
  - default: `true`, means _emit a warning when there is a whitespace beyond the configured `Limit`_.
    When `false`, allows include items such as long URLs without being forced to break them in the middle

## Example

```erlang
{elvis_text_style, line_length, #{ limit => 100
                                 , skip_comments => false
                                 , no_whitespace_after_limit => true
                                 }}
```
