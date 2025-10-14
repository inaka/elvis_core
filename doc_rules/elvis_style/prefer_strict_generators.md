# Prefer Strict Generators [![](https://img.shields.io/badge/since-4.2.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.2.0)

Within comprehensions, avoid `<-` and `<=` in favor of `<:-` and `<:=`.

## Avoid

```erlang
[ X || X <- a:list()],
<< <<X>> || X <= a:binary() >>
#{K => V || K := V <- a:map()}
```

## Prefer

```erlang
[ X || X <:- a:list()],
<< <<X>> || X <:= a:binary() >>
#{K => V || K := V <:- a:map()}
```

## Rationale

Quoting OTP's official documentation:
> Using strict generators is a better practice when either strict or relaxed generators work.
> More details are in [Programming Examples](https://www.erlang.org/doc/system/list_comprehensions).

## Options

- None.

## Example configuration

```erlang
{elvis_style, prefer_strict_generators, #{}}
```
