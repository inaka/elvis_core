# No Match in Condition

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Don't write code like this:

```erlang
case #{a := A} = do:something() of
    #{b := good} -> {a, really, nice, A};
    #{b := bad} -> {"not", a, good, A}
end
```

Do the matching in the clause heads (i.e., outside of the `case` condition):

```erlang
case do:something() of
    #{a := A, b := good} -> {a, really, nice, A};
    #{a := A, b := bad} -> {"not", a, good, A}
end
```

While the code as written in the first example is valid, it's much harder to understand
(particularly for large statements) than the one from the second example.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_match_in_condition, #{}}
```
