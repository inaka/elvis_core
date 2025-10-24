# Strict Module Layout [![](https://img.shields.io/badge/since-4.2.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.2.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Prefer sticking an exact order of the different parts in a module.

## Avoid

```erlang
-module(…).

-dialyzer(…).

-include[_lib](…).
```

> with a configuration that describes an order: [module, include, dialyzer]

## Prefer

```erlang
-module(…).

-include[_lib](…).

-dialyzer(…).
```

> with a configuration that describes an order: [module, include, dialyzer]

## Rationale

This is a readability issue. It can improve the odds of others reading and
liking your code by making it easier to follow.

## Options

- order :: [atom]
  - default: [module, include, dialyzer, elvis, mixin, type_attr, export_type, export, function]

- ignore_mod_parts :: [atom]
  - default :: []

## Example configuration

```erlang
{elvis_style, strict_module_layout,
  #{order => [module, inclue, dialyzer, type, export_type, export, function_call]}}
```
