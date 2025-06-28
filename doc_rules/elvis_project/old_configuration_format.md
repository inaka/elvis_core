# Old Configuration Format

`elvis.config` files should not use an outdated format.

> [!WARNING]
> This rule is now deprecated, since a warning is issued and default options are assumed when
> the configuration is not a map (for older rules).

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
