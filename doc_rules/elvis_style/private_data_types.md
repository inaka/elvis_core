# Private Data Types

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Exporting functions' internally-defined data types, in order to consume those
types externally, results in tightly-coupled code. Modules should be responsible
for defining their own internal data types. If these are needed outside the
modules, they should be made
[opaque](https://www.erlang.org/doc/reference_manual/opaques.html).

> "Works on `.beam` file? Yes!"

## Options

- `applies_to :: [record | map | tuple]`
  - default: `record`.

## Example

```erlang
{elvis_style, private_data_types}
```
