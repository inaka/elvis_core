# Prefer Idiomatic Calls ![](https://img.shields.io/badge/BEAM-yes-orange)

Prefer Erlang syntax and literals where they express an operation more directly than a function
call. This rule is opt-in. Each warning includes the suggested syntax and any conditions under
which it applies. For example:

```text
for the call 'maps:update/3', consider Map#{Key := Value}.
```

## Calls and alternatives

The default configuration contains 11 function tuples, each paired with replacement guidance:

| Call | Alternative shown in the warning |
|------|---------------------------------|
| `maps:put/3` | `Map#{Key => Value}` |
| `maps:update/3` | `Map#{Key := Value}` |
| `string:equal/2` | `Left =:= Right` for strings with the same flat representation |
| `string:concat/2` | `Left ++ Right` for character lists |
| `string:is_empty/1` | `String =:= []` for flat lists or `String =:= <<>>` for binaries |
| `lists:append/2` | `Left ++ Right` |
| `maps:find/2` | A map pattern such as `#{Key := Value} = Map` when the result is immediately matched |
| `maps:merge/2` | `Map#{Key => Value}` for a map literal, or remove a merge with `#{}` |
| `erlang:length/1` | `List =:= []` or pattern matching on `[]` and `[_ \| _]` when only checking emptiness |
| `erlang:list_to_binary/1` | A binary literal when converting a string literal |
| `erlang:atom_to_list/1` | A string literal when converting an atom literal |

Preferences for another library function, a more efficient API, or different options are outside
this rule's scope. For example, it does not recommend `sets:from_list(List, [{version, 2}])` over
`sets:from_list(List)`, or `ets:lookup_element/3` over `ets:lookup/2`. Those are API or performance
choices, rather than preferences for language syntax.

Calls already covered by `no_common_caveats_call`, such as `erlang:list_to_atom/1` and
`erlang:binary_to_atom/1,2`, are omitted from the default list. `erlang:atom_to_binary/1,2` also
remains allowed by default.

Comprehension-based replacements are also outside the current scope. Calls such as `lists:map/2`,
`maps:map/2`, `maps:from_list/1`, and `maps:to_list/1` remain allowed by default.

Calls are checked throughout the syntax tree, including guards and binary segment values and
sizes. The rule matches module, function, and arity. It does not verify the conditions described
in the guidance, so it can report a call that is appropriate in its context. Function references
and dynamic calls are not reported by the default configuration.

## Rationale

Map syntax, operators, pattern matching, and literals can express familiar operations directly.
This rule concerns that style preference. A separate rule lets projects configure it independently
of custom restrictions in `no_call` and performance or behavior concerns in `no_common_caveats_call`.

Warnings invite review, rather than an automatic replacement. Preserve the intended behavior:

- `string:equal/2` and `string:is_empty/1` support mixed and nested character data. Strict equality
  compares the representation too, and nested empty character data may be neither `[]` nor `<<>>`.
- Use `:=` for `maps:update/3` to preserve its missing-key error; `=>` would insert a missing key.
- Keep `length/1` when the actual length is needed, and conversion functions when their inputs
  are computed values. The syntax suggestions apply only in the circumstances named in the warning.

See the [Erlang maps guide](https://www.erlang.org/doc/system/maps.html),
[lists documentation](https://www.erlang.org/doc/apps/stdlib/lists.html), and
[string documentation](https://www.erlang.org/doc/apps/stdlib/string.html).

## Options

- `idiomatic_functions` is a list of `{FunctionSpec, [Alternative]}` pairs. `FunctionSpec` must be
  an exact `{Module, Function, Arity}` tuple. `Alternative` is the replacement guidance shown in
  the warning, stored in a single-element argument list. The default is the 11 pairs documented
  above. Supplying this option replaces the default list.
- The standard `ignore` option excludes modules or enclosing functions; its default is `[]`.

## Example configuration

```erlang
{elvis_style, prefer_idiomatic_calls, #{}}
```

To check only map insertion and update calls, with their replacement guidance:

```erlang
{elvis_style, prefer_idiomatic_calls, #{
    idiomatic_functions => [
        {{maps, put, 3}, ["Map#{Key => Value}"]},
        {{maps, update, 3}, ["Map#{Key := Value}"]}
    ]
}}
```

To exclude a function that intentionally uses one of these calls:

```erlang
{elvis_style, prefer_idiomatic_calls, #{ignore => [{my_module, compare_strings, 2}]}}
```
