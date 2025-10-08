# Max Map Type Keys

The number of fields in a type definition that describes a map should be
limited to a defined maximum.

**Note**: Maps with infinite keys (i.e., those that use anything but
atoms as keys) are ignored by this rule. The idea is to focus on the
map types that represent system entities, database records, etc.

## Rationale

Limiting the number of keys in type definitions improves readability and
maintainability. Types with too many properties tend to represent too complex
structures that can generally be subdivided or split into more appropriate
ones that are easier to understand and work with.
Keeping map types concise encourages clear,
focused logic and makes it easier to navigate the codebase.

## Options

- `max_keys :: non_neg_integer()`
  - default: `25`

## Example configuration

```erlang
{elvis_style, max_map_type_keys, #{ max_keys => 30 }}
```
