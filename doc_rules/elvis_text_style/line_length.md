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

## Example

```erlang
{elvis_text_style, line_length, #{ limit => 100
                                 , skip_comments => false
                                 }}
```
