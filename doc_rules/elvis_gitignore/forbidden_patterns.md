# `.gitignore` forbidden patterns

(since [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0))

Some patterns in `.gitignore` should be excluded.

## Rationale

This is a simple convention for consistency. The patterns mentioned in the options (default)
are those a majority of bootstrap projects should not adopt.

## Options

- `regexes :: [string()]`
  - default: `["^rebar.lock$"]`

## Example configuration

```erlang
{elvis_gitignore, forbidden_patterns, #{ regexes => ["^rebar.lock$"] }}
```
