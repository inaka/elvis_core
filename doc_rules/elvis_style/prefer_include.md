# Prefer Include [![](https://img.shields.io/badge/since-4.2.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.2.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Using `include_lib("myinc.hrl")` is unnecessary if the parameter is a single file.
In that case, using `include("myinc.hrl")` is exactly the same.
This rule prefers `include` over `include_lib` to make them consistent.

## Avoid

```erlang
include_lib("myinc.hrl")
```

## Prefer

```erlang
include("myinc.hrl")
```

or

```erlang
include_lib("mydir/myinc.hrl")
```

## Rationale

`-include("file.hrl")` is exactly equivalent to `-include_lib("file.hrl")`. The only difference is that `include_lib`
can be used to include files from another directory than the default one.o, for example: `-include_lib("somewhere/header.hrl")`.
It will attempt to look the file up in the application directory of "somewhere".

## Options

- None.

## Example configuration

```erlang
{elvis_style, prefer_include, #{}}
```
