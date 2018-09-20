# elvis-core [![Build Status](https://travis-ci.org/inaka/elvis_core.svg?branch=master)](https://travis-ci.org/inaka/elvis_core) [![Stories in Ready](https://badge.waffle.io/inaka/elvis.png?label=ready&title=Ready)](https://waffle.io/inaka/elvis)

Erlang style reviewer core library.

This library includes the mechanism to apply rules to your code and their
implementation.

## Contact Us
If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

## Contributing & Reporting Bugs

If you find a bug or want to contribute to this project please create an issue through
the [elvis](https://github.com/inaka/elvis) repository. This is where all elvis related
work is coordinated.

## Usage

### As a library

This library implements all the core functionality for the
[`elvis`](https://github.com/inaka/elvis) command-line tool. For an example on
how to use it please check that project.

### Erlang Shell

After adding `elvis_core` as a dependency to your project and setting up its
[configuration](#configuration), you can run it from an Erlang shell in the
following two ways.

```erlang
elvis_core:rock().
%%+ # src/elvis_core.erl [OK]
%%+ # src/elvis_result.erl [OK]
%%+ # src/elvis_style.erl [OK]
%%+ # src/elvis_utils.erl [OK]
%%= ok
```

This will try to load the configuration for `elvis_core` specified in an
`elvis.config` located in the current directory. If no configuration is
found `invalid_config` will be thrown.

To start the application in the shell enter the following command:

```erlang
application:start(elvis).
%%= ok
```

Another option for using `elvis_core` from the shell is explicitly providing a
configuration as an argument to `rock/1`:

```erlang
Config = [#{dirs => ["src"], filter => "*.erl", rules => []}],
elvis_core:rock(Config).
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

We have only presented results where all files were well-behaved (respect all
the rules), so here's an example of how it looks when files break some of the
rules:

```
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

To provide a default configuration you should create an `elvis.config` file located
in the root directory:

```erlang
[
 {
   elvis,
   [
    {config,
     [#{dirs => ["src"],
        include_dirs => ["include"],
        filter => "*.erl",
        ruleset => erl_files
       },
      #{dirs => ["."],
        filter => "Makefile",
        ruleset => makefiles
       },
      #{dirs => ["."],
        filter => "rebar.config",
        ruleset => rebar_config
       },
      #{dirs => ["."],
        filter => "elvis.config",
        ruleset => elvis_config
       }
     ]
    },
    %% Optional to select the output format, the default is colors
    {output_format, plain},
    %% Only necessary for the 'webhook' functionality
    {github_user, "user"},
    {github_password, "password"}
   ]
 }
].
```

The `include_dirs` key is just a list of folders (`[string()]`) where to search for
header files.

The `dirs` key is a list that indicates where `elvis` should look for the
files that match `filter`, which will be run through each of the default rules
in the specified `ruleset`, which is an *atom*. If you want to override the
[default rules](https://github.com/inaka/elvis_core/blob/master/src/elvis_rulesets.erl)
for a given ruleset you need to specify them in a `rules` key which is a
list of items with the following structure `{Module, Function, RuleConfig}` or
`{Module, Function}` if the rule takes no configuration values. You can also
`disable` certain rules if you want to just by specifying the rule in the `rules`
key and passing `disable` as its third parameter.

**IMPORTANT:** `disable` will only work if you also provided a `ruleset` as shown above.

Let's say you like your files to have a maximum of 90 characters per line and
also you like to use tabs instead of spaces, so you need to override `erl_files`
`ruleset` default `rules` as follows:

```erlang
#{dirs => ["src"],
  filter => "*.erl",
  rules => [{elvis_style, line_length, #{limit => 90}}, %% change default line_length limit from 100 to 90
            {elvis_style, no_tabs, disable}], %% disable no_tabs rule
  ruleset => erl_files
},
```

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

The implementation of a rule is just a function that takes 3 arguments: `elvis`'s
`config` entry from its [configuration](#configuration); the file to be
analyzed; and a configuration map specified for the rule. This means you can
define rules of your own as long as the functions that implement them respect
this arity.

You can find the default `elvis` configuration file at `config/elvis.config`.

The GitHub configuration parameters `github_user` and `github_password` are
required only when `elvis` is used as a [webhook](#webhook).

## Implemented Rules

A reference of all rules implemented in Elvis can be found in this wiki page:
[Rules](https://github.com/inaka/elvis/wiki/Rules).

## References

Inspired by [HoundCI][houndci]

  [houndci]: https://houndci.com/
  [config]: http://www.erlang.org/doc/man/config.html
