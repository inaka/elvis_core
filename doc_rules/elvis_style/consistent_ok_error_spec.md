# Consistent Ok/Error Spec [![](https://img.shields.io/badge/since-5.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/5.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

If a `-spec` declaration includes an `{ok, ...}` tuple as possible result, it should also include
possible error results.

## Avoid

```erlang
-spec some_function(input()) -> {ok, output()}.
```

## Prefer

```erlang
-spec some_function(input()) -> {ok, output()} | {error, error()}.
```

or:

```erlang
-spec some_function(input()) -> output().
```

## Rationale

Wrapping up results in an `{ok, …}` tuple doesn't serve any purpose other than leading its users
to think that the function may also return errors. If that's the case, then those errors should
be part of the funciton spec. If it's not, then it's better for consistency, composition, and
refactoring to just return the expected output, without wrapping it up.

## Options

- None.

## Example configuration

```erlang
{elvis_style, consistent_ok_error_spec, #{}}
```
