# elvis-core [![Build Status](https://github.com/inaka/elvis_core/workflows/build/badge.svg)](https://github.com/inaka/elvis_core) [![Stories in Ready](https://badge.waffle.io/inaka/elvis.png?label=ready&title=Ready)](https://waffle.io/inaka/elvis)

Erlang style reviewer core library.

This library includes the mechanism to apply rules to your code and their
implementation.

## Contributing & Reporting Bugs

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

The [elvis](https://github.com/inaka/elvis) repository is where all elvis_core
related work is coordinated.

## Usage

### As a library

This library implements all the core functionality for analyzing files and
applying the styles rules defined here or custom ones defined by its users.

The [`elvis`](https://github.com/inaka/elvis) command-line tool uses it
extensively, so please check that project for a concrete example on how
you could use for your own purposes.

### Erlang Shell

After adding `elvis_core` as a dependency to your project and starting a
shell, you will need to make sure the application is started:

```erlang
{ok, _} = application:ensure_all_started(elvis_core).
%%= {ok,[ zipper, katana_code, elvis_core]}
```

Once this is done you can run the style rules in the following ways.

#### Load configuration from a file

```erlang
ElvisConfig = elvis_config:from_file("elvis.config").
elvis_core:rock(ElvisConfig).
%%+ # src/elvis_core.erl [OK]
%%+ # src/elvis_result.erl [OK]
%%+ # src/elvis_style.erl [OK]
%%+ # src/elvis_utils.erl [OK]
%%= ok
```

This will try to load the [configuration](#configuration) specified in an
`elvis.config` located in the current directory. If no configuration is found
`invalid_config` will be thrown.

#### Provide configuration as a value

Another option for using `elvis_core` from the shell is explicitly providing a
configuration as an argument to `rock/1`:

```erlang
ElvisConfig = [#{dirs => ["src"], filter => "*.erl", rules => []}],
elvis_core:rock(ElvisConfig).
%%+ # src/elvis_core.erl [OK]
%%+ # src/elvis_result.erl [OK]
%%+ # src/elvis_style.erl [OK]
%%+ # src/elvis_utils.erl [OK]
%%= ok
```

**IMPORTANT**: `Config` should have a valid format, but since this is a project
under development the definition for *valid format* is still a work in progress.
If the configuration format changes though, the example configuration files and
the documentation in this README will be updated.

#### Output for failing rules

We have only presented results where all files were well-behaved (respect all
the rules), so here's an example of how it looks when files break some of the
rules:

```erlang
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

An `elvis.config` configuration file should look like this:

```erlang
[ { elvis
  , [ { config
      , [ #{ dirs    => ["src"]
           , filter  => "*.erl"
           , ruleset => erl_files
           }
        , #{ dirs    => ["."]
           , filter  => "Makefile"
           , ruleset => makefiles
           }
        , #{ dirs    => ["."]
           , filter  => "rebar.config"
           , ruleset => rebar_config
           }
        , #{ dirs    => ["."]
           , filter  => "elvis.config"
           , ruleset => elvis_config
           }
        ]
      }
      %% output_format (optional): how to format the output.
      %% Possible values are 'plain', 'colors' or 'parsable' (default='colors').
    , {output_format, colors}
      %% verbose (optional): when 'true' more information will
      %% be printed (default=false).
    , {verbose, true}
      %% no_output (optional): when 'true' nothing will be printed
      %% (default=false).
    , {no_output, false}
      %% parallel: determine how many files in parallel will be
      %% analyzed (default=1).
    , {parallel, 1}
    ]
  }
].
```

### Files, Rules & Rulesets

The `dirs` key is a list that indicates where `elvis` should look for the
files that match `filter`, which will be run through each of the default rules
in the specified `ruleset`, which is an *atom*.

If you want to override the [default rules][default-rules] for a given ruleset
you need to specify them in a `rules` key which is a list of items with the
following structure `{Module, Function, RuleConfig}` or `{Module, Function}`
if the rule takes no configuration values. You can also `disable` certain rules
if you want to just by specifying the rule in the `rules` key and passing
`disable` as its third parameter.

#### Disabling Rules

**IMPORTANT:** `disable` will only work if you also provided a `ruleset` as shown above.

Let's say you like your files to have a maximum of 90 characters per line and
also you like to use tabs instead of spaces, so you need to override `erl_files`
`ruleset` default `rules` as follows:

```erlang
#{dirs => ["src"],
  filter => "*.erl",
  rules => [{elvis_text_style, line_length, #{limit => 90}}, %% change default line_length limit from 100 to 90
            {elvis_text_style, no_tabs, disable}], %% disable no_tabs rule
  ruleset => erl_files
},
```

#### Ignoring Modules

You can also `ignore` modules at a _check level_ or at a _ruleset (group of checks) level_:

- at check level by setting the ignore parameter in the rule you want to skip, e.g:

  ```erlang
  {elvis_style, no_debug_call, #{ignore => [elvis, elvis_utils]}}
  ```

  There we are telling elvis to **ignore** _elvis_ and _elvis_utils_ modules when running `no_debug_call` check.

- at ruleset level by setting the **ignore** group level option for the group you want to skip, e.g:

  ```erlang
  #{dirs => ["src"],
    filter => "*.erl",
    ruleset => erl_files,
    ignore => [module1, module4]
   }
  ```

  With this configuration, none of the checks for [erl_files](https://github.com/inaka/elvis_core/blob/master/src/elvis_rulesets.erl#L6-L34) would be applied to `module1.erl` and `module4.erl` files.

### Formating

`output_format` is used to configure the output format. Possible values are `colors`,
`plain` and `parsable`. The latter could be use for the automated parsing and has a
format very close to dialyzer in a form `FILE:LINE:RULE:MESSAGE`:

```erlang
src/example.erl:1:god_modules:This module has too many functions (56). Consider breaking it into a number of modules.
src/example_a.erl:341:no_debug_call:Remove the debug call to io:format/2 on line 341.
src/example_a.erl:511:used_ignored_variable:Ignored variable is being used on line 511 and column 54.
src/example_a.erl:1252:used_ignored_variable:Ignored variable is being used on line 1252 and column 21.
```

### Verbosity

It is possible to indicate elvis_core to produce more information in its output
with the `verbose` option. The value provided should be a boolean, either `true`
or `false`.

The default value for `verbose` is `false`.

On the other hand if no output is desired then the value for the  `no_output`
option should be `true`.

The default value for `no_output` is `false`.

### Parallelization

In order to speed up the analysis of files, the `parallel` option can be used.

Its value indicates how many processes to use at the same time to apply the
style rules to all the files gathered. The number provided should be less than
or equal to the available CPUs, since any value higher than that won't report
any speedup benefits.

The default value for `parallel` is `1`.

## Configuration Examples

You can find examples for  configuration files in this project's `config` directory.

## Implemented Rules

A reference of all rules implemented in Elvis can be found in this wiki page:
[Rules](https://github.com/inaka/elvis_core/wiki/Rules).

### Custom Rules

The implementation of a rule is just a function that takes 3 arguments: `elvis`'s
`config` entry from its [configuration](#configuration); the file to be
analyzed; and a configuration map specified for the rule. This means you can
define rules of your own as long as the functions that implement them respect
this arity.

## References

Inspired by [HoundCI][houndci]

[default-rules]: (https://github.com/inaka/elvis_core/blob/master/src/elvis_rulesets.erl)
[houndci]: https://houndci.com/
[config]: http://www.erlang.org/doc/man/config.html
