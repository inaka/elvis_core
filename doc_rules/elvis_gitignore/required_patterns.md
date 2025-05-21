# `.gitignore` required patterns [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0)

Some patterns in `.gitignore` should be included.

## Rationale

This is a simple convention for consistency. The patterns mentioned in the options (default)
are those a majority of bootstrap projects should adopt.

## Options

- `regexes :: [string()]`
  - default: `["^.rebar3/$",
               "^_build/$",
               "^_checkouts/$",
               "^doc/$",
               "^/erl_crash.dump$",
               "^/rebar3.crashdump$",
               "^test/logs/$"]`

## Example configuration

```erlang
{elvis_gitignore, required_patterns, #{ regexes => ["^.rebar3/$"
                                                  , "^_build/$"
                                                  , "^_checkouts/$"
                                                  , "^doc/$"
                                                  , "^/erl_crash.dump$"
                                                  , "^/rebar3.crashdump$"
                                                  , "^test/logs/$"
                                                   ]
                                      }}
```
