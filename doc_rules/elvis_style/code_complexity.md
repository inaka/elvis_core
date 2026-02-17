# Code Complexity

Functions should not have excessively high cyclomatic complexity.

Cyclomatic complexity counts the number of decision points in a function. It starts at 1
and increments for each additional branch: extra function clauses, `case`/`if`/`receive`/`try`/`maybe`
clauses (beyond the first), and `andalso`/`orelse` operators.

Anonymous functions are not counted as part of the enclosing function's complexity.

## Avoid

```erlang
complex_handler(Request) ->
    case parse(Request) of
        {ok, Data} ->
            if
                is_valid(Data) andalso is_authorized(Data) ->
                    case process(Data) of
                        {ok, Result} ->
                            try
                                format(Result)
                            catch
                                error:badarg -> {error, format};
                                _:_ -> {error, unknown}
                            end;
                        {error, R} -> {error, R}
                    end;
                true ->
                    {error, invalid}
            end;
        {error, Reason} ->
            {error, Reason}
    end.
```

## Prefer

Break complex functions into smaller, focused functions:

```erlang
handle(Request) ->
    case parse(Request) of
        {ok, Data} -> process_valid(Data);
        {error, Reason} -> {error, Reason}
    end.

process_valid(Data) ->
    case is_valid(Data) andalso is_authorized(Data) of
        true -> process_and_format(Data);
        false -> {error, invalid}
    end.

process_and_format(Data) ->
    case process(Data) of
        {ok, Result} -> safe_format(Result);
        {error, R} -> {error, R}
    end.
```

## Rationale

High cyclomatic complexity indicates many branching paths through a function, making it
harder to understand, test, and maintain. Breaking complex functions into smaller pieces
improves readability and testability.

## Options

- `max_complexity :: pos_integer()`
  - default: `10`

## Example configuration

```erlang
{elvis_style, code_complexity, #{ max_complexity => 10 }}
```
