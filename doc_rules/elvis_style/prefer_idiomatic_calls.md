# Prefer Idiomatic Calls ![](https://img.shields.io/badge/BEAM-yes-orange)

Suggest more idiomatic alternatives to selected library calls. This rule is opt-in.
It matches the module, function, and arity; it does not inspect arguments or surrounding code.

## Avoid/prefer

| Call | Alternative to consider |
|------|-------------------------|
| `maps:put(Key, Value, Map)` | `Map#{Key => Value}` |
| `maps:update(Key, Value, Map)` | `Map#{Key := Value}` |
| `string:equal(Left, Right)` | `Left =:= Right` for strings with the same representation |
| `string:concat(Left, Right)` | `Left ++ Right` |
| `string:is_empty(String)` | `String =:= []` or `String =:= <<>>` for flat strings |
| `lists:append(Left, Right)` | `Left ++ Right` |
| `ets:lookup(Table, Key)` | `ets:lookup_element(Table, Key, Position)` when only one tuple element is needed |
| `lists:map(Fun, List)` | A list comprehension when it expresses the transformation clearly |

The following calls are also checked, based on ELP's style and performance diagnostics:

| Call | Alternative to consider |
|------|-------------------------|
| `maps:find/2` | Map pattern matching when the lookup result is immediately inspected |
| `maps:merge/2` | Map update syntax when merging a map literal, or remove an empty-map merge |
| `maps:from_list/1` | A map comprehension when the input is a list comprehension |
| `maps:to_list/1` | A map generator when converting a map just to iterate over it |
| `lists:foldl/3` | `maps:from_list/1` or `maps:from_keys/2` for simple map-building folds |
| `erlang:length/1` | Pattern matching for emptiness checks, or `lists:flatlength/1` after flattening |
| `lists:flatten/1` | `lists:flatlength/1` when only the flattened length is needed |
| `lists:reverse/1` | `lists:reverse/2` before concatenation, or `lists:last/1` before head extraction |
| `erlang:hd/1`, `lists:nth/2` | `lists:last/1` when extracting the head of a reversed list |
| `lists:zip/2`, `lists:seq/2,3` | `lists:enumerate/1,2,3` when building indexed lists |
| `string:lowercase/1`, `binary:encode_hex/1` | `binary:encode_hex(Data, lowercase)` when lowercasing encoded hex |
| `erlang:list_to_binary/1`, `erlang:list_to_atom/1` | A binary or atom literal when converting a string literal |
| `erlang:atom_to_list/1`, `erlang:atom_to_binary/1` | A string or binary literal when converting an atom literal |
| `sets:new/0`, `sets:from_list/1` | Explicit version 2 options on OTP releases where version 1 is the default |

The default list contains 29 function tuples. `maps:map/2`, other arities, function references,
and dynamic calls remain allowed with this configuration. Calls are checked throughout the syntax
tree, including guards and binary segment values and sizes.

ELP restricts many of these diagnostics to particular expressions or OTP versions. This rule
reports every call matching a configured tuple, including calls that are appropriate in their context.
It does not implement ELP's argument or surrounding-expression checks. Some suggested replacement
functions, such as `maps:from_list/1`, are themselves listed because other uses may be simplified.

## Rationale

Operators, map syntax, and comprehensions can express common operations more directly. A separate
rule lets projects enable these preferences independently of `no_call` and
`no_common_caveats_call`, and leaves room for argument-specific checks in the future.

The warnings suggest reviewing a call, rather than applying an automatic replacement. Keep the
original call where it better expresses the intent or has different behavior:

- `string:equal/2` and `string:is_empty/1` support mixed and nested character data. Strict equality
  compares the representation too, and nested empty character data may be neither `[]` nor `<<>>`.
- `ets:lookup/2` returns complete objects and an empty list for a missing key. `ets:lookup_element/3`
  extracts a tuple element and raises `badarg` for a missing key. Consider the table type and error
  handling before replacing a lookup.
- `lists:map/2` can be clearer with a named callback. A normal comprehension with a patterned
  generator also skips unmatched elements, whereas a patterned callback may raise an error.
- Use `:=` for `maps:update/3` to preserve its missing-key error; `=>` would insert a missing key.

See the [Erlang maps guide](https://www.erlang.org/doc/system/maps.html),
[string documentation](https://www.erlang.org/doc/apps/stdlib/string.html), and
[ETS documentation](https://www.erlang.org/doc/apps/stdlib/ets.html#lookup_element/3).
The additional tuples are drawn from ELP's
[registered linters](https://github.com/WhatsApp/erlang-language-platform/blob/1fddb80f07b10136bd524abd41789f3de14382fe/crates/ide/src/diagnostics.rs#L2225).

## Options

- `idiomatic_functions` selects the calls to report. Its default is the 29 tuples documented above.
  Entries can be `{Module, Function, Arity}` or `{Module, Function}`, with `'_'` wildcards supported.
  Supplying this option replaces the default list.
- The standard `ignore` option excludes modules or enclosing functions; its default is `[]`.

## Example configuration

```erlang
{elvis_style, prefer_idiomatic_calls, #{}}
```

To check only map insertion and update calls:

```erlang
{elvis_style, prefer_idiomatic_calls, #{
    idiomatic_functions => [{maps, put, 3}, {maps, update, 3}]
}}
```

To exclude a function that intentionally uses one of these calls:

```erlang
{elvis_style, prefer_idiomatic_calls, #{ignore => [{my_module, compare_strings, 2}]}}
```
