# `elvis_core` [![GitHub Actions CI](https://github.com/inaka/elvis_core/workflows/build/badge.svg)](https://github.com/inaka/elvis_core/actions) [![Erlang Support](https://img.shields.io/badge/Erlang/OTP-26+-blue)](https://www.erlang.org)

`elvis_core` is the core library for the [`elvis`](https://github.com/inaka/elvis) Erlang style
reviewer. It is also used by [`rebar3_lint`](https://github.com/project-fifo/rebar3_lint) for easier
integration into your Erlang libraries or applications.

It includes the mechanisms to apply rules to your Erlang code, as well as their implementation.

It implements [pre-defined rules](#pre-defined-rules), but also supports
[user-defined ones](#user-defined-rules).

## Usage

### As a command-line tool

The [`elvis`](https://github.com/inaka/elvis) command-line tool uses `elvis_core` extensively, so
do check that project for a concrete example on how you could use it for your own purposes.

### As a `rebar3` plugin

The [`rebar3_lint`](https://github.com/project-fifo/rebar3_lint) plugin eases integration of the
style reviewer into your application or library. Be sure to check that out for further information.

### From the Erlang shell

After adding `elvis_core` as a dependency to your project and starting a shell, you will need to
make sure the application is started:

```shell
1> {ok, _} = application:ensure_all_started(elvis_core).
{ok,[zipper,katana_code,elvis_core]}
2>
```

Once this is done you can apply the style rules in the following ways.

#### Loading configuration from a file

```shell
1> elvis_core:rock({config_file, "elvis.config"}).
Loading src/elvis_code.erl
# src/elvis_code.erl [OK]
Loading src/elvis_config.erl
# src/elvis_config.erl [OK]
Loading src/elvis_core.erl
# src/elvis_core.erl [OK]
Loading src/elvis_file.erl
# src/elvis_file.erl [OK]
...
ok
2>
```

This will load the [configuration](#configuration), specified in file `elvis.config`, from the
current directory.

If `elvis.config` is not present, the application will fall back to searching for configuration
parameters in `rebar.config`. If `rebar.config` is also unavailable, the application proceeds to
perform a tertiary lookup within its application environment (which can also be set via the
`app/sys.config` file, or e.g., via `application:set_env(elvis_core, Key, Value).` for the required
settings.

#### Providing configuration as a value

Another option for using `elvis_core` from the shell is to explicitly provide the configuration as
an argument to `elvis_core:rock/1`:

```shell
1> ElvisConfig = [#{files => ["src/elvis_rule.erl"], ruleset => erl_files}].
[#{files => ["src/elvis_rule.erl"],rules => []}]
2> elvis_core:rock({config, ElvisConfig}).
Loading src/elvis_rule.erl
# src/elvis_rule.erl [OK]
ok
3>
```

#### Output for failing rules

We have only presented results where all files were well-behaved (i.e. they respect all the rules),
so here's an example of how the output looks when files break some of the rules:

```shell
# test/examples/british_behaviour_spelling.erl [FAIL]
  - state_record_and_type (https://github.com/inaka/elvis_core/tree/main/doc_rules/elvis_style/state_record_and_type.md)
    - This module implements an OTP behavior but is missing a '#state{}' record.
# test/examples/fail_always_shortcircuit.erl [FAIL]
  - always_shortcircuit (https://github.com/inaka/elvis_core/tree/main/doc_rules/elvis_style/always_shortcircuit.md)
    - At line 5, column 45, unexpected non-shortcircuiting operator 'or' was found; prefer 'orelse'.
```

## Configuration

An `elvis.config` file looks something like this:

```erlang
[
    {config, [
        #{
            files => ["src/*.erl"],
            ruleset => erl_files
        },
        #{
            files => ["include/*.hrl"],
            ruleset => hrl_files
        },
        #{
            files => ["rebar.config"],
            ruleset => rebar_config
        }
    ]},
    % output_format (optional): how to format the output.
    % Possible values are 'plain', 'colors' or 'parsable' (default='colors').
    {output_format, colors},
    % verbose (optional): when 'true' more information will
    % be printed (default=false).
    {verbose, true},
    % no_output (optional): when 'true' nothing will be printed
    % (default=false).
    {no_output, false},
    % parallel: determine how many files will be
    % analyzed in parallel (default=1).
    {parallel, 1}
].
```

To look at what is considered the "current default" configuration, do:

```console
rebar3 shell
...
1> elvis_config:default().
[#{files => ["apps/**/src/*.erl","src/*.erl"],
   ruleset => erl_files},
 #{files =>
       ["apps/**/src/*.hrl","apps/**/include/*.hrl",
        "src/*.hrl","include/*.hrl"],
   ruleset => hrl_files},
 #{files => ["rebar.config"],
   ruleset => rebar_config},
 #{files => [".gitignore"],
   ruleset => gitignore}]
2>
```

**Note**: this element might change with time. The above was what was generated when this
documentation was updated.

### Files, rules and rulesets

The `files` key is a list of glob patterns that tell `elvis_core` which files to analyse. Each
pattern uses [`filelib:wildcard/1`](https://erlang.org/doc/man/filelib.html#wildcard-1), so you can
use patterns like `"src/*.erl"` or `"apps/**/src/*.erl"`. Matching files are run through the
pre-defined rules in the specified `ruleset`.

If you want to override the [pre-defined rules](#pre-defined-rules), for a given ruleset, you need
to specify them in a `rules` key which is a list of items with the following structure
`{RuleNamespace, Rule, RuleConfig}`, or `{RuleNamespace, Rule}` - if the rule takes no configuration
values. You can also `disable` certain rules if you want to, by specifying them in the `rules` key
and passing `disable` as a third argument.

`RuleNamespace` is an Erlang module that implements the `elvis_rule` behaviour.
`Rule` is a function exported from `RuleNamespace`.

#### Disabling Rules

**IMPORTANT**: `disable` will only work if you also provided a `ruleset` as shown above.

Let's say you like your files to have a maximum of 90 characters per line and you also like to use
tabs instead of spaces. In that case, you need to override `erl_files`'s `ruleset` pre-defined
`rules` as follows:

```erlang
#{
    files => ["src/*.erl"],
    rules => [
        % change max_line_length from 100 to 90
        {elvis_text_style, max_line_length, #{limit => 90}},
        % disable no_tabs
        {elvis_text_style, no_tabs, disable}
    ],
    ruleset => erl_files
}.
```

#### Ignoring modules

You can also `ignore` modules at a _check level_ or at a _ruleset (group of checks) level_:

- at a _check level_, you set the `ignore` option in the rule you want to ignore, e.g.:

```erlang
{elvis_style, no_debug_call, #{ ignore => [elvis, elvis_utils, "not_a_module.hrl"] }}
```

(we are telling `elvis` to **ignore** the `elvis` and `elvis_utils` modules and the
`not_a_module.hrl` file (as a string, since it's not a module) when executing the
`no_debug_call` check. You can also use wildcard patterns: `'_'` in any position matches any
module, function, or arity (e.g. `[{mod, '_'}]` for all functions in a module, `[{'_', fun}]` for a
function in any module).

- at a _ruleset (group of checks) level_, you set the `ignore` option for the group you want to
ignore, e.g.:

```erlang
#{
    files => ["src/*.erl"],
    ruleset => erl_files,
    ignore => [module1, module4, "path/to/header_file.hrl"]
}.
```

With this configuration, none of the checks for `erl_files` is applied to `module1`, `module4`,
or `path/to/header_file.hrl`.

##### `.gitignore`

In addition to custom ignore rules, `.gitignore` patterns are automatically honored during
execution to skip unnecessary paths.

### Formatting

Option `output_format` allows you to configure the output format. Possible values are `colors`,
`plain` and `parsable`. The latter could be used for automated parsing and has a format very close
to the one presented by `dialyzer`, like `<file>:<line>:<rule>:<message>`:

<!-- markdownlint-disable MD013 -->
```shell
test/examples/british_behaviour_spelling.erl:-1:state_record_and_type:This module implements an OTP behavior but is missing a '#state{}' record.```
test/examples/fail_always_shortcircuit.erl:5:always_shortcircuit:At line 5, column 45, unexpected non-shortcircuiting operator ''or'' was found; prefer 'orelse'.
```
<!-- markdownlint-enable MD013 -->

The default value for the `output_format` option is `colors`.

### Verbosity

It is possible to tell `elvis_core` to produce a more verbose output, using the `verbose` option.
The value provided is a boolean, either `true` or `false`.

The default value for the `verbose` option is `false`.

On the other hand, if no output is desired then the value for the `no_output` option should be
`true`.

The default value for the `no_output` option is `false`.

### Parallel execution

In order to speed up the analysis process, you can use the `parallel` option.

Its value indicates how many processes to use at the same time to apply the style rules to all the
files gathered. The provided number should be less than or equal to the available cores, since any
value higher than that won't report any speedup benefits.

The default value for `parallel` is `1`.

### Warnings-as-errors

The `warnings_as_errors` parameter determines how the analysis execution influences your system's
exit code. This is particularly useful for governing build pipelines.

- `true` (default): any detected warning or error will cause the process to fail with a non-zero
exit code. Use this to enforce strict code quality standards.

- `false`: the analysis will print all findings to the console, but the process will return an exit
code of 0. This allows the build to continue even if issues are found.

## Configuration examples

You can find examples for configuration files in this project's
[`config`](https://github.com/inaka/elvis_core/tree/HEAD/config) directory.

## Application environment

Options `output_format`, `verbose`, `no_output`, and `parallel` can also be set as application-level
environment variables, i.e. as they would be found by `application:get_env/2,3`.

## Rules

### Pre-defined rules

A reference to all pre-defined rules (and some other information) implemented in `elvis_core` can be
found in this repository's [RULES.md](https://github.com/inaka/elvis_core/blob/main/RULES.md).

### User-defined rules

To implement your own rules, see the [User-defined rules](RULES.md#user-defined-rules) section in
[RULES.md](RULES.md) for the full interface, APIs, and a complete example.

## Versioning

From 5.x, `elvis_core` adheres to [Semantic Versioning](https://semver.org/). All breaking changes
and upgrade paths for major versions are documented in [`MIGRATION.md`](https://github.com/inaka/elvis_core/blob/main/MIGRATION.md).

## Contributing and reporting bugs

If you find any **bugs** or have other **problems** using this library,
[open an issue](https://github.com/inaka/elvis/issues/new) in this repository (or even a pull
request 😃).

## References

Inspired by [HoundCI](https://houndci.com/).
