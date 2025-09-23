# Max Function Arity [![](https://img.shields.io/badge/since-3.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/3.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

The number of arguments in a non-anonymous function definition should be limited to a defined
maximum.

## Avoid

```erlang
request(Method, Host, Port, Path, Query, Headers, Body, Timeout, Retries, ExpectedCode) ->
    ...
end.
```

## Prefer

A potential solution is to refactor the function by grouping related arguments into maps, or
custom data structures. This reduces the arity of the function while preserving clarity and intent:

```erlang
request(Method, Target, Data, ReqOptions, RespOptions) ->
    #{
        host := Host,
        port := Port,
        path := Path,
        query := Query
     } = Target,
    #{
        headers := Headers,
        body := Body
     } = Data,
    #{
        timeout := Timeout,
        retries := Retries
     } = ReqOptions,
    #{
        expected_code := ExpectedCode
     } = RespOptions,
    ...
end.
```

## Rationale

Limiting the number of function arguments improves readability, maintainability, and testability.
Functions with too many parameters often indicate insufficient encapsulation. Refactoring such
functions into smaller, more focused units or using records, maps, or structured data types can
lead to cleaner and more manageable code.

## Options

- `max_arity :: non_neg_integer()`
  - default: `8`
- `non_exported_max_arity :: non_neg_integer() | same` [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0)
  - default: `8`

## Example configuration

```erlang
{elvis_style, max_function_arity, #{ max_arity => 8
                                   , non_exported_max_arity => 8
                                   }}
```
