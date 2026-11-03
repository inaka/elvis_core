# Prefer Idiomatic Calls ![](https://img.shields.io/badge/BEAM-yes-orange)

Prefer Erlang syntax and literals over equivalent function calls. This rule is opt-in and suggests
the following alternatives by default:

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

Calls are matched by module, function, and arity throughout the syntax tree, including guards and
binary segment values and sizes. Function references and dynamic calls are not reported by default.

## Rationale

Map syntax, operators, patterns, and literals can express these operations more directly. API,
performance, and comprehension rewrites are outside this rule's scope. See [No Call](no_call.md)
for custom restrictions and [No Common Caveats](no_common_caveats_call.md) for known pitfalls.

The rule does not check whether a suggested replacement applies. Review each warning:

- `string:equal/2` and `string:is_empty/1` support mixed and nested character data. Strict equality
  compares the representation too, and nested empty character data may be neither `[]` nor `<<>>`.
- Use `:=` for `maps:update/3` to preserve its missing-key error; `=>` would insert a missing key.
- Keep `length/1` when the length is needed, and conversion functions for computed inputs.

## Options

- `idiomatic_functions :: [{{module(), function(), arity()}, [string()]}]`
  - default: the 11 calls and alternatives above.
  - Each exact call tuple is paired with one suggestion string in a single-element list.
    Setting this option replaces the default list.
- `ignore` excludes modules or enclosing functions (default: `[]`).

## Example configuration

```erlang
% Default
{elvis_style, prefer_idiomatic_calls, #{}}

% Check only map insertion and update calls
{elvis_style, prefer_idiomatic_calls, #{
    idiomatic_functions => [
        {{maps, put, 3}, ["Map#{Key => Value}"]},
        {{maps, update, 3}, ["Map#{Key := Value}"]}
    ]
}}

% Ignore an enclosing function
{elvis_style, prefer_idiomatic_calls, #{ignore => [{my_module, compare_strings, 2}]}}
```
