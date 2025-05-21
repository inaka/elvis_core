# Export Used Types

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Types used in typespecs for exported functions should also be exported.

> Works on `.beam` file? Yes!

## Avoid

```erlang
-module(mylib).

-export([myfun/1]).
-type mytype() :: term().

-spec myfun(mytype()) -> ok.
myfun(Var) ->
    ok = do_something_with(Var).
```

## Prefer

Note the use of `export_type` in the example below.

```erlang
-module(mylib).

-export([myfun/1]).
-type mytype() :: term().
-export_type([mytype/0]).

-spec myfun(mytype()) -> ok.
myfun(Var) ->
    ok = do_something_with(Var).
```

## Rationale

Exporting a function without exporting the types it depends on can lead to redundant type
definitions in each module that uses those functions. To prevent this, when a function is exported,
its dependent types should also be exported.

## Options

- None.

## Example configuration

```erlang
{elvis_style, export_used_types, #{}}
```
