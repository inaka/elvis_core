# Macro Names

Macro names should be written in SCREAMING_SNAKE_CASE.

## Avoid

> This is a convention aimed at ensuring consistency, rather than a coding issue.

```erlang
-define('myMacro\'sDaBomb!'(), io:format("My macro's da bomb!~n")).
      % ^_macro name______^

myfun() -> ?'myMacro\'sDaBomb!'().
```

## Prefer

```erlang
-define(MY_MACROS_DA_BOMB(), io:format("My macro's da bomb!~n")).

myfun() -> ?MY_MACROS_DA_BOMB().
```

**Note**: this convention avoids the use of single quotes (`'`), improving readability and reducing
visual clutter.

## Rationale

Although macros can technically be named using any valid atom syntax - including quoted atoms - most
Erlang/OTP documentation adopts the screaming snake case convention recommended by Elvis as
the standard.

## Options

- `regex :: string()`. [![](https://img.shields.io/badge/since-1.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/1.0.0)
  - default: `"^[A-Z](_?[A-Z0-9]+)*$"`

`regex` was `"^([A-Z][A-Z_0-9]+)$"` until [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0).

## Example configuration

```erlang
{elvis_style, macro_names, #{ regex => "^[A-Z](_?[A-Z0-9]+)*$" }}
```
