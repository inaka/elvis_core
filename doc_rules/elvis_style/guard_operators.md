# Guard Operators [![](https://img.shields.io/badge/since-4.2.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.2.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Avoid using a mix of `andalso`, `orelse`, `,`, and `;`, by choosing a consistent coding style for
guard expressions.

## Avoid

```erlang
case some:expression() of
    X when X == a; X =< 10 -> {clause, 1};
    X when X == b orelse X == 10 -> {clause, 2};
    X when (X == c orelse X == d); X >= 10 -> {clause, 3}
end
```

## Prefer

```erlang
case some:expression() of
    X when X == a; X =< 10 -> {clause, 1};
    X when X == b; X == 10 -> {clause, 2};
    X when X == c; X == d; X >= 10 -> {clause, 3}
end
```

Or…

```erlang
case some:expression() of
    X when X == a orelse X =< 10 -> {clause, 1};
    X when X == b orelse X == 10 -> {clause, 2};
    X when X == c orelse X == d orelse X >= 10 -> {clause, 3}
end
```

Or, at least…

```erlang
case some:expression() of
    X when X == a; X =< 10 -> {clause, 1};
    X when X == b orelse X == 10 -> {clause, 2};
    X when X == c; X == d; X >= 10 -> {clause, 3}
end
```

(where _within each guard_ the choice of operators is consistent)

## Rationale

Choosing a specific guard operator style enhances consistency across your codebase.

## Options

- `preferred_syntax :: punctuation | words | per_expression | per_clause`
  - default: `per_expression`
  - `punctuation` - always use `,` or `;`.
  - `words` - always use `andalso` or `orelse`.
  - `per_expression` - do not mix _words_ and _punctuation_ in an expression.
  - `per_clause` - do not mix _words_ and _punctuation_ in a guard.

## Example configurations

Using these functions as an example:

```erlang
first_function(X) when X == a; X =< 10 -> {clause, 1};
first_function(X) when X == b orelse X == 10 -> {clause, 2};
first_function(X) when (X == c orelse X == d); X >= 10 -> {clause, 3}.

second_function(X) when X == a; X =< 10 -> {clause, 1};
second_function(X) when X == b; X == 10 -> {clause, 2};
second_function(X) when X == c; X == d; X >= 10 -> {clause, 3}.

third_function(X) when X == a orelse X =< 10 -> {clause, 1};
third_function(X) when X == b orelse X == 10 -> {clause, 2};
third_function(X) when X == c orelse X == d orelse X >= 10 -> {clause, 3}.
```

```erlang
{elvis_style, guard_operators, #{ preferred_syntax => punctuation }}
```

This configuration will emit a warning for:

- clauses 2 and 3 in the first function
- all clauses in the third function

```erlang
{elvis_style, guard_operators, #{ preferred_syntax => words }}
```

This configuration will emit a warning for:

- clauses 1 and 3 in the first function
- all clauses in the second function

```erlang
{elvis_style, guard_operators, #{ preferred_syntax => per_expression }}
```

This configuration will emit a warning for:

- clauses 2 and 3 in the first function

```erlang
{elvis_style, guard_operators, #{ preferred_syntax => per_clause }}
```

This configuration will emit a warning for:

- clause 3 in the first function
