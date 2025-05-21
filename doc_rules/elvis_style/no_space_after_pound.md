# No Space after `#`

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

Spaces after the `#` symbol in maps or records should be avoided.

> Works on `.beam` file? No.

## Quick fix

Use an Erlang code formatter that enforces strict spacing.

## Avoid

```erlang
BadMap = #   {this => map, has => spaces, af_ter => pound}.
BadRecord =     #      this_record{has = spaces, af_ter = pound}.
```

## Prefer

```erlang
GoodMap = #{this => map, has => no_spaces, af_ter => pound}.
GoodRecord =     #this_record{has = no_spaces, af_ter = pound}.
```

## Rationale

The `#` symbol is used for both records (`#record{}`) and maps (`#{}`).

While adding a space after `#` is not syntactically incorrect, it introduces visual ambiguity and
reduces code readability. Inconsistent formatting can make it harder to distinguish between
records, maps, and general usage, especially in larger codebases. Enforcing no space after `#`
promotes consistency, improves clarity, and avoids potential confusion during code reviews or
maintenance.

## Options

- None

## Example configuration

```erlang
{elvis_style, no_space_after_pound, #{}}
```
