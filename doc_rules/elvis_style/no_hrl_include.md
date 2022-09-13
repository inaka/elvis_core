# Prevent including *.hrl file 

The idea behind this rule comes from [this email](http://erlang.org/pipermail/erlang-questions/2017-April/092112.html) by @kvakvs.
Basically, the warning is emitted if a developer _forgets a comma_ and writes something like the following:

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