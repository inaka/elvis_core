# Private Data Types

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Exporting a function's internal data type in order to construct those types
externally results in tightly-coupled code. A module's should be responsible
for creating its own internal types. So, if the data type is needed outside the
module, it should be
[an opaque type](https://www.erlang.org/doc/reference_manual/opaques.html).

> "Works on `.beam` file? Yes!"

## Options

- `applies_to :: [atom()]`
  - default: `record`.
  - other options: `map` and `tuple`.

## Example

```erlang
{elvis_style, private_data_types}
```
