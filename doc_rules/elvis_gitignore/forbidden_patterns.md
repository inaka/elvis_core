# `.gitignore` forbidden patterns

(since [3.3.0](https://github.com/inaka/elvis_core/releases/tag/3.3.0))

Exclude, from the project's `.gitignore` file, the patterns identified by the rule.

## Options

- `regexes :: [string()]`.
  - default: `["^rebar.lock$"]`.

## Example

```erlang
{elvis_gitignore, forbidden_patterns, #{}}
```
