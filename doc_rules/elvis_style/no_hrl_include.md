# Prevent including *.hrl file

With this rule we can raise warning/errors if developer includes restricted `*.hrl` files in non-allowed modules



```erlang
-module(user).
-include("private.hrl").
.............
```

It will raise style warning/error for above module 

## Options

- None.

## Example

```erlang
{elvis_style, no_hrl_include, #{restricted_hrl_files => ["private.hrl"]}}
```