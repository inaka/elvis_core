# No Types

(since [2.1.0](https://github.com/inaka/elvis_core/releases/tag/2.1.0))

Avoid `-type` attributes.

This rule is meant to be used on header files only.
Defining types in public header files (especially those intended for inclusion via -include_lib())
might lead to type name clashes between projects and even modules of a single big project.
Instead, types should be defined in the modules which they correspond to
(using `-export_type()` appropriately) and, in this way, take advantage of the namespacing offered
by module names.
In other words, this rule means that we will always need to use `some_mod:some_type()` unless
we're referring to a type defined in the same module.

> Works on `.beam` file? Yes, but it's not useful there. This rule is meant to be used for header files.

## Options

- None.

## Example

```erlang
{elvis_style, no_types}
```
