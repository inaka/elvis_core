# Old Configuration Format [![](https://img.shields.io/badge/until-4.1.0-red)](https://github.com/inaka/elvis_core/releases/tag/4.1.0)

`elvis.config` files should not use an outdated format.

> #### Warning {: .warning}
>
> This rule was removed in version [4.2.0](https://github.com/inaka/elvis_core/releases/tag/4.2.0).

## Avoid

```erlang
[
    {elvis, [
        {config, [
            #{
                dirs => ["src/**"],
                filter => "*.erl",
                ruleset => erl_files,
                rules => [{elvis_style, no_god_modules, [25]}]
                                                      % ^ old format used lists
            }
        ]}
    ]}
].
```

## Prefer

```erlang
[
    {elvis, [
        {config, [
            #{
                dirs => ["src/**"],
                filter => "*.erl",
                ruleset => erl_files,
                rules => [{elvis_style, no_god_modules, #{limit => 25}}]
            }
        ]}
    ]}
].
```

## Rationale

Older versions of the `elvis.config` format were harder to read and maintain. The newer format
simplifies structure and uses maps (`#{}`) for rule options, improving clarity and reducing the
chance of configuration errors. Keeping the configuration up to date ensures compatibility with the
latest Elvis features and best practices.

## Options

- None.

## Example configuration

```erlang
{elvis_project, old_configuration_format, #{}}
```
