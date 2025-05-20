# Old configuration format

`elvis.config` files should not use an outdated format.

## Avoid

```erlang
[
    {elvis, [
        {config, [
            #{
                dirs => ["src/**"],
                filter => "*.erl",
                ruleset => erl_files,
                rules => [{elvis_style, god_modules, [25]}]
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
                rules => [{elvis_style, god_modules, #{limit => 25}}]
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

## Example

```erlang
{elvis_project, old_configuration_format, #{}}
```
