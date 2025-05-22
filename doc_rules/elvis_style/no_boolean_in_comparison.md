# No Boolean In Comparison [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Explicit comparisons of expressions to boolean literals (`true` or `false`) should be avoided.

## Avoid

```erlang
mycondition(Assumptions) == true

mycondition(Assumptions) == false
```

## Prefer

```erlang
mycondition(Assumptions)

not(mycondition(Assumptions))
```

## Rationale

Explicitly comparing expressions to boolean literals (e.g., `Expr == true` or `Expr == false`) is
redundant and non-idiomatic in Erlang. Erlang's control flow constructs (such as `if`, `case`,
guards, and boolean operators like `andalso`/`orelse`) are designed to evaluate the truthiness of
expressions directly. Using direct comparisons can obscure intent, reduce readability, and increase
the risk of subtle bugs - especially in cases where expressions yield non-boolean but truthy values
(e.g., atoms or tuples). Favoring direct boolean evaluation leads to clearer, more idiomatic, and
more maintainable code.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_boolean_in_comparison, #{}}
```
