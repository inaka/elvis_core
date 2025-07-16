# Prefer Include [![](https://img.shields.io/badge/since-4.2.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.2.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Using `-include_lib("myinc.hrl").` when including a single file, should be avoided.

## Avoid

```erlang
-include_lib("myinc.hrl").
```

## Prefer

```erlang
-include("myinc.hrl").
```

## Rationale

`-include("myinc.hrl").` and `-include_lib("myinc.hrl").` are functionally equivalent when
referencing files in the same application. The primary difference is that `-include_lib` allows
inclusion of headers from other applications by resolving the path relative to the specified
application's ebin directory. For example, `-include_lib("elsewhere/theirinc.hrl").` will search
for `theirinc.hrl` in the elsewhere application's include path.

When including headers within the same application, using `-include` is simpler and avoids
unnecessary indirection.
This improves readability and reduces confusion about file location.

## Options

- None.

## Example configuration

```erlang
{elvis_style, prefer_include, #{}}
```
