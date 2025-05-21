# Max Anonymous Function Arity

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

The number of arguments in an anonymous function definition should be limited to a defined maximum.

> Works on `.beam` file? Yes

## Avoid

```erlang
caller() ->
    fun(Method, Host, Port, Path, Query, Headers, Body, Timeout, Retries, ExpectedCode) ->
        ...
    end.
```

## Prefer

A potential solution is to refactor the function by grouping related arguments into maps, or
custom data structures. This reduces the arity of the function while preserving clarity and intent:

```erlang
caller() ->
    fun(Method, Target, Data, ReqOptions, RespOptions) ->
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
  - default: `5`

## Example configuration

```erlang
{elvis_style, max_anonymous_function_arity, #{ max_arity => 5 }}
```
