# Prefer Sigils [![](https://img.shields.io/badge/since-5.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/5.0.0)

Since **Erlang/OTP 27+**, prefer
[sigil syntax](https://www.erlang.org/blog/highlights-otp-27/#sigils) for UTF-8 text that would
otherwise be written as a **plain binary string literal** (a single `string` segment inside
`<< >>`). The rule does nothing on older OTP releases.

## Avoid

```erlang
<<"hello">>
```

(and other `<<"...">>` forms that contain only one string segment—the kind the rule flags.)

## Prefer

```erlang
~"hello"
```

Multiline and other delimiters follow the general sigil rules; for example:

```erlang
~"""
hello
world
"""
```

## Rationale

Sigils (see [EEP 66](https://www.erlang.org/eeps/eep-0066.html)) make string and binary literals
easier to read and to refactor: you can pick delimiters to reduce escaping, express multiline text
clearly, and align with modern Erlang style. Binary literals built from a single string segment are
a natural place to adopt them first.

## Options

- None.

## Example configuration

```erlang
{elvis_style, prefer_sigils, #{}}
```
