# `.gitignore` forbidden patterns

(since [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0))

Exclude, from the project's `.gitignore` file, the patterns identified by the rule.

## Options

- `regexes :: [string()]`.
  - default: `["^rebar.lock$"]`.

## Example

```erlang
{elvis_gitignore, forbidden_patterns, #{}}
```
