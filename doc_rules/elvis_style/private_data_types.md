# Private Data Types

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Types based on records, maps or tuples should be exported as
[opaque](https://www.erlang.org/doc/reference_manual/opaques.html).

> Works on `.beam` file? Yes!

## Avoid

```erlang
-type person_rec() :: #person{name :: string(), age :: integer()}.
-type person_map() :: #{ name := string(), age := integer() }.

-export_type([person_rec/0]).
-export_type([person_map/0]).
```

## Prefer

**Note**: replace `-type` with `-opaque`

```erlang
-opaque person_rec() :: #person{name :: string(), age :: integer()}.
-opaque person_map() :: #{ name := string(), age := integer() }.

-export_type([person_rec/0]).
-export_type([person_map/0]).
```

## Rationale

Types based on records, maps, or tuples expose the internal structure of the data, which can
compromise modularity and flexibility. By marking these types as opaque, we prevent external
modules from directly accessing or modifying the underlying data structure. This enhances
encapsulation, making it clear that the structure should only be manipulated via provided
functions, rather than allowing direct access to its internals.

This approach is especially useful when types are defined using records or maps, which are often
used to represent complex data structures. By exporting them as opaque, we protect the module’s
internal details and can later refactor the underlying implementation without breaking the public
API.

## Options

- `apply_to :: [record | map | tuple]`
  - default: `[record]`

## Example configuration

```erlang
{elvis_style, private_data_types, #{ apply_to => [record] }}
```
