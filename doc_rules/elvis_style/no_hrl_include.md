# No hrl include

This rule raises a warning if specific `hrl` files are included in chosen modules.



```erlang
-module(user).
-include("private.hrl").
.............
```

E.g. the following will raise a warning for `private.hrl` being included by module `user`.

## Options

- None.

## Example

```erlang
{elvis_style, no_hrl_include, #{ restricted_hrl_files => ["private.hrl"] }}
```