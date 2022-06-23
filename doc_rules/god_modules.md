# God Modules

There shouldn't be any modules that export a number of functions greater than the specified limit.

> Works on `.beam` file? Yes!

## Options

- `limit :: non_neg_integer().`
  - default: `25`

## Example

```erlang
{elvis_style, god_modules, #{ limit => 25 }}
```
