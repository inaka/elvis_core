# Line Length

Lines should not exceed the defined length limit.

## Exceptions

Comment lines may be excluded from this constraint, with option `skip_comments`.

## Quick fix

Use an Erlang code formatter that enforces strict line length limitations.

## Options

- `limit :: pos_integer()`
  - default: `100`
- `skip_comments :: false | any | whole_line`
  - default: `false` - emits a warning for every line exceeding the limit
  - `any` - no warning is emitted if the portion of the line exceeding the limit is part of a
  comment
  - `whole_line` - no warning is emitted if the entire line that exceeds the limit consists solely
  of a comment.
- `no_whitespace_after_limit :: boolean()`
  - default: `true` - emits a warning when there is whitespace beyond the configured limit
  - `false` - allows exceptions, such as long URLs, to extend beyond the limit without requiring
  line breaks

## Example configuration

```erlang
{elvis_text_style, line_length, #{ limit => 100
                                 , skip_comments => false
                                 , no_whitespace_after_limit => true
                                 }}
```
