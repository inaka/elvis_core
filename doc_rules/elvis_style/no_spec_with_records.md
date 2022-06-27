# No Spec With Records

Don't use records in `-spec(_).`s; use types that map to records instead.

```erlang
%% Don't do
-spec some_function(#rec{}) -> ok.

%% Do
-type rec() :: #rec{}.
-spec some_function(rec()) -> ok.
```

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_spec_with_records}
%% or
{elvis_style, no_spec_with_records, #{}}
```
