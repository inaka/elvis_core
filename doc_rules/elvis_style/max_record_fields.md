# Max Record Fields

The number of fields in a record definition should be limited to a defined maximum.

## Rationale

Limiting the number of fields in record definitions improves readability and
maintainability. Records with too many fields tend to represent too complex
structures that can generally be subdivided or split into more appropriate
ones that are easier to understand and work with.
Keeping records concise encourages clear,
focused logic and makes it easier to navigate the codebase.

## Options

- `max_fields :: non_neg_integer()`
  - default: `25`

## Example configuration

```erlang
{elvis_style, max_record_fields, #{ max_fields => 30 }}
```
