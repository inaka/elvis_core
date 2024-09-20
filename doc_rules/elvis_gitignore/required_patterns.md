# `.gitignore` required patterns

(since [3.3.0](https://github.com/inaka/elvis_core/releases/tag/3.3.0))

Include, in the project's `.gitignore` file, the patterns identified by the rule.

## Options

- `regexes :: [string()]`.
  - default: `["^.rebar3/$",
               "^_build/$",
               "^_checkouts/$",
               "^doc/$",
               "^/erl_crash.dump$",
               "^/rebar3.crashdump$",
               "^test/logs/$"]`.

## Example

```erlang
{elvis_gitignore, required_patterns, #{}}
```
