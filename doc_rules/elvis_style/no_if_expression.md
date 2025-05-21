# No If Expression

`if` expressions should not be used.

> Works on `.beam` file? Yes!

## Avoid

```erlang
my_function(X) ->
    if
        X > 10 -> ok;
        true -> error
    end.
```

## Prefer

```erlang
my_function(X) ->
    case X > 10 of
      true -> ok;
      _Else -> error
    % ^ no binding required
    end.
```

## Rationale

`if` expressions should be avoided in Erlang code because they are not as predictable or
maintainable as other control flow mechanisms, such as `case`. The `if` expression in Erlang lacks
an explicit `else` branch, which can lead to runtime errors if no condition is matched.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_if_expression, #{}}
```
