# elvis_core [![GitHub Actions CI][ci-img]][ci]

[ci]: https://github.com/inaka/elvis_core
[ci-img]: https://github.com/inaka/elvis_core/workflows/build/badge.svg

`elvis_core` is the core library for the [`elvis`](https://github.com/inaka/elvis) Erlang style
reviewer. It is also used by [`rebar3_lint`](https://github.com/project-fifo/rebar3_lint) for easier
integration into your Erlang libraries or applications.

It includes the mechanisms to apply rules to your Erlang code, as well as their implementation.

It implements [pre-defined rules](#pre-defined-rules), but also supports
[user-defined ones](#user-defined-rules).

## Usage

### As a library

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
1> ElvisConfig = elvis_config:from_file("elvis.config").
<loaded_config>
2> elvis_core:rock(ElvisConfig).
# src/elvis_core.erl [OK]
# src/elvis_result.erl [OK]
# src/elvis_style.erl [OK]
# src/elvis_utils.erl [OK]
ok
3>
```

This will load the [configuration](#configuration), specified in file `elvis.config`, from the
current directory. If no configuration is found `{invalid_config, _}` is thrown.

#### Providing configuration as a value

Another option for using `elvis_core` from the shell is to explicitly provide the configuration as
an argument to `rock/1`:

```shell
1> ElvisConfig = [#{dirs => ["src"], filter => "*.erl", rules => []}].
[#{dirs => ["src"],filter => "*.erl",rules => []}]
2> elvis_core:rock(ElvisConfig).
Loading src/elvis_core.erl
# src/elvis_core.erl [OK]
Loading src/elvis_result.erl
# src/elvis_result.erl [OK]
Loading src/elvis_style.erl
# src/elvis_style.erl [OK]
Loading src/elvis_utils.erl
# src/elvis_utils.erl [OK]
ok
3>
```

#### Output for failing rules

We have only presented results where all files were well-behaved (i.e. they respect all the rules),
so here's an example of how the output looks when files break some of the rules:

```shell
# ../../test/examples/fail_line_length.erl [FAIL]
  - line_length
    - Line 14 is too long: "    io:format(\"This line is 81 characters long and should be detected, yeah!!!\").".
    - Line 20 is too long: "    io:format(\"This line is 90 characters long and should be detected!!!!!!!!!!!!!!!!!!\").".
# ../../test/examples/fail_no_tabs.erl [FAIL]
  - no_tabs
    - Line 6 has a tab at column 0.
    - Line 15 has a tab at column 0.
# ../../test/examples/small.erl [OK]
```

## Configuration

An `elvis.config` file looks something like this:

```erlang
[{elvis, [
    {config, [
        #{ dirs    => ["src"]
         , filter  => "*.erl"
         , ruleset => erl_files }
      , #{ dirs    => ["."]
         , filter  => "rebar.config"
         , ruleset => rebar_config }
      , #{ dirs    => ["."]
         , filter  => "elvis.config"
         , ruleset => elvis_config }
    ]}
    % output_format (optional): how to format the output.
    % Possible values are 'plain', 'colors' or 'parsable' (default='colors').
  , {output_format, colors}
    % verbose (optional): when 'true' more information will
    % be printed (default=false).
  , {verbose, true}
    % no_output (optional): when 'true' nothing will be printed
    % (default=false).
  , {no_output, false}
    % parallel: determine how many files will be
    % analyzed in parallel (default=1).
  , {parallel, 1}
]}].
```

### Files, rules and rulesets

The `dirs` key is a list that tells `elvis_core` where it should look for the files that match
`filter`, which will be run through each of the pre-defined rules in the specified `ruleset`.
`filter` can contain `**` for further matching (it uses
[`filelib:wildcard/1`](https://erlang.org/doc/man/filelib.html#wildcard-1) under the hood).

If you want to override the [pre-defined rules](#pre-defined-rules), for a given ruleset, you need
to specify them in a `rules` key which is a list of items with the following structure
`{Module, Function, RuleConfig}`, or `{Module, Function}` - if the rule takes no configuration
values. You can also `disable` certain rules if you want to, by specifying them in the `rules` key
and passing `disable` as a third parameter.

#### Disabling Rules

**IMPORTANT**: `disable` will only work if you also provided a `ruleset` as shown above.

Let's say you like your files to have a maximum of 90 characters per line and you also like to use
tabs instead of spaces. In that case, you need to override `erl_files`'s `ruleset` pre-defined
`rules` as follows:

```erlang
#{ dirs => ["src"]
 , filter => "*.erl"
 , rules => [
       {elvis_text_style, line_length, #{ limit => 90 }} % change line_length from 100 to 90
     , {elvis_text_style, no_tabs, disable} % disable no_tabs
   ]
 , ruleset => erl_files
 }.
```

#### Ignoring modules

You can also `ignore` modules at a _check level_ or at a _ruleset (group of checks) level_:

- at a _check level_, you set the `ignore` option in the rule you want to ignore, e.g.:

```erlang
{elvis_style, no_debug_call, #{ ignore => [elvis, elvis_utils] }}
```

(we are telling `elvis` to **ignore** the `elvis` and `elvis_utils` modules when executing
the `no_debug_call` check.

- at a _ruleset (group of checks) level_, you set the `ignore` option for the group you want to
ignore, e.g.:

```erlang
#{ dirs => ["src"]
 , filter => "*.erl"
 , ruleset => erl_files
 , ignore => [module1, module4]
}.
```

With this configuration, none of the checks for `erl_files` is applied to `module1` or `module4`.

### Formatting

Option `output_format` allows you to configure the output format. Possible values are `colors`,
`plain` and `parsable`. The latter could be used for automated parsing and has a format very close
to the one presented by `dialyzer`, like `<file>:<line>:<rule>:<message>`:

<!-- markdownlint-disable MD013 -->
```shell
src/example.erl:1:god_modules:This module has too many functions (56). Consider breaking it into a number of modules.
src/example_a.erl:341:no_debug_call:Remove the debug call to io:format/2 on line 341.
src/example_a.erl:511:used_ignored_variable:Ignored variable is being used on line 511 and column 54.
src/example_a.erl:1252:used_ignored_variable:Ignored variable is being used on line 1252 and column 21.
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

## Configuration examples

You can find examples for configuration files in this project's
[`config`](https://github.com/inaka/elvis_core/tree/HEAD/config) directory.

## Application environment

Options `output_format`, `verbose`, `no_output`, and `parallel` can also be set as application-level
environment variables, i.e. as they would be found by `application:get_env/2,3`.

## Rules

### Pre-defined rules

A reference to all pre-defined rules (and some other information) implemented in `elvis_core` can be
found in this repository's [RULES.md](RULES.md).

### User-defined rules

The implementation of a new rule is a function that takes 3 arguments in the following order:

1. `elvis_config:config()`: the value of option `config` as found in the
[configuration](#configuration),
1. `elvis_file:file()`: the file to be analyzed,
1. `map()`: a configuration map specific to your user-defined rule.

This means you can define rules of your own (user-defined rules) as long as the functions that
implement them respect this interface.

## Contributing and reporting bugs

If you find any **bugs** or have other **problems** using this library,
[open an issue](https://github.com/inaka/elvis/issues/new) in this repository (or even a pull
request 😃).

## References

Inspired by [HoundCI](https://houndci.com/).
