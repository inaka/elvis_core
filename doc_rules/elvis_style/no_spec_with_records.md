# No Spec With Records ![](https://img.shields.io/badge/BEAM-yes-orange)

Records should not be used in `-spec` declarations; use types that map to records instead.

## Avoid

```erlang
-spec some_function(#rec{}) -> ok.
some_function(_Rec) -> ok.
```

## Prefer

```erlang
-type rec() :: #rec{}.
-spec some_function(rec()) -> ok.
some_function(_Rec) -> ok.
```

## Rationale

Using records in `-spec` declarations can introduce unnecessary dependencies and complexity, as it
forces importing the record definitions into the module. This undermines the principle of
abstraction, as the implementation details of records should be hidden behind functions that create
or manipulate them.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_spec_with_records, #{}}
```
