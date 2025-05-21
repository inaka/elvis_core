# Protocol for deps

(since [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0)

`rebar.config` dependencies should use known protocols.

## Avoid

```erlang
{deps, [
    {elvis_core, {git, "git@github.com:inaka/elvis_core.git", {tag, "4.0.0"}}}
]}.
```

## Prefer

```erlang
{deps, [
    {elvis_core, {git, "https://github.com/inaka/elvis_core.git", {tag, "4.0.0"}}}
]}.
```

## Rationale

Using an unknown or incorrect protocol (e.g., `foo`) might cause `rebar3` to fail during dependency
resolution. Ensuring that only recognized protocols are used makes builds reliable and avoids
obscure errors during fetching or compilation. It also increases consistency across your codebase.

## Options

- `regex :: string()`
  - default: `^(https://|git://|\\d+(\\.\\d+)*)`

## Example

```erlang
{elvis_project, protocol_for_deps, #{ regex => "^(https://|git://|\\d+(\\.\\d+)*)" }}
```
