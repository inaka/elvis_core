# No Import [![](https://img.shields.io/badge/since-3.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/3.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

`import` attributes should not be used.

## Avoid

```erlang
-import(lists, [map/2]).

process(Items, Fun) ->
    map(Fun, Items).
```

## Prefer

```erlang
process(Items, Fun) ->
    lists:map(Fun, Items).
```

## Rationale

The use of `-import` attributes reduces code clarity by making it harder to determine
which module a function originates from. This can lead to name collisions, reduce readability,
and increase maintenance complexity - especially in large codebases.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_import, #{}}
```
