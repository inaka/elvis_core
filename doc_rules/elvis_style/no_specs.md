# No Specs

(since [2.1.0](https://github.com/inaka/elvis_core/releases/tag/2.1.0))

Avoid `-spec` attributes.

This rule is meant to be used on header files only.
Defining specs in public header files (especially those intended for inclusion via `-include_lib(...)`)
might lead to spec clashes between projects and even modules of a single big project.
Instead, specs should be defined next to the functions they're specifying.

> Works on `.beam` file? Yes, but it's not useful there. This rule is meant to be used for header files.

## Options

- None.

## Example

```erlang
{elvis_style, no_specs}
```
