<!-- markdownlint-disable MD033 -->
# No <code>&&nbsp;</code>

(since [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0))

Use of <code>$&nbsp;</code> should be avoided.

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected)

## Avoid

```erlang
case Char of
    $ -> "It's a space";
    _ -> "It's something else"
end.
```

## Prefer

```erlang
case Char of
    $\s -> "It's a space";
    _ -> "It's something else"
end.
```

## Rationale

The use of <code>$&nbsp;</code> to represent character spaces reduces code readability and clarity.

## Options

- None.

## Example

```erlang
{elvis_style, no_dollar_space, #{}}
```
