# Nesting Level

Nesting levels in code structures should be limited to a specified maximum depth.

> Works on `.beam` file? Yes!

## Avoid

```erlang
-export([handle_request/1]).

handle_request(Request) ->
    case parse_request(Request) of
        {ok, Parsed} ->
            case validate(Parsed) of
                true ->
                    case fetch_data(Parsed) of
                        {ok, Data} ->
                            case process_data(Data) of
                                {ok, Result} ->
                                    {reply, Result};
                                {error, Reason} ->
                                    {error, {processing_failed, Reason}}
                            end;
                        {error, NotFound} ->
                            {error, {data_not_found, NotFound}}
                    end;
                false ->
                    {error, invalid_request}
            end;
        {error, Reason} ->
            {error, {bad_request, Reason}}
    end.
```

## Prefer

After some refactoring, this might become:

```erlang
-export([handle_request/1]).

handle_request(Request) ->
    case parse_request(Request) of
        {ok, Parsed} ->
            handle_parsed(Parsed);
        {error, Reason} ->
            {error, {bad_request, Reason}}
    end.

handle_parsed(Parsed) ->
    case validate(Parsed) of
        true ->
            fetch_and_process(Parsed);
        false ->
            {error, invalid_request}
    end.

fetch_and_process(Parsed) ->
    case fetch_data(Parsed) of
        {ok, Data} ->
            process_result(Data);
        {error, NotFound} ->
            {error, {data_not_found, NotFound}}
    end.

process_result(Data) ->
    case process_data(Data) of
        {ok, Result} ->
            {reply, Result};
        {error, Reason} ->
            {error, {processing_failed, Reason}}
    end.
```

## Rationale

Deeply nested code is harder to read, understand, test, and maintain. Limiting nesting levels
encourages clearer logic, flatter control structures, and promotes refactoring into smaller,
more focused functions. This improves overall code quality and reduces cognitive load for
developers.

## Options

- `level :: pos_integer()`
  - default: `4` (prior to [0.7.0](https://github.com/inaka/elvis_core/releases/tag/0.7.0) was `3`)

## Example configuration

```erlang
{elvis_style, nesting_level, #{ level => 4 }}
```
