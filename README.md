[![Stories in Ready](https://badge.waffle.io/inaka/elvis.png?label=ready&title=Ready)](https://waffle.io/inaka/elvis)

# elvis-core

Erlang style reviewer core library.

This library includes the mechanism to apply rules to your code and their
implementation.

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).

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
application:start(elvis_core).
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
...
#{dirs => ["src"],
  filter => "*.erl",
  rules => [{elvis_style, line_length, #{limit => 90}}, %% change default line_length limit from 80 to 90
            {elvis_style, no_tabs, disable}], %% disable no_tabs rule
  ruleset => erl_files
},
...
```

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
