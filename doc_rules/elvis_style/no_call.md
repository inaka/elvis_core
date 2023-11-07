# No call

(since [0.4.0](https://github.com/inaka/elvis_core/releases/tag/0.4.0))

This rule raise a warning when certain functions are called. It is also used internally to implement
`no_debug_call` and `no_common_caveats` but, on its own, makes no checks on your code (the default
`no_call_functions` list is empty).  However, it is a convenient place to add your own list of calls
to avoid (especially calls to third party libraries, where you can't just deprecate undesired
functions).

**Notice**: this rule is not enforced by default. Check the
[example `elvis.config` file](../../README.md#configuration) to see how you can enforce it.

> Works on `.beam` file? Yes!

## Options

- `no_call_functions :: [{module() | '_', function() | '_', arity() | '_'} |
  {module() | '_', function() | '_'}]`.
  - default: `[]`.

`'_'` wildcards supported since [3.2.0](https://github.com/inaka/elvis_core/releases/tag/3.2.0)

## Example

```erlang
{elvis_style, no_call, #{ no_call_functions => [] }}
```
