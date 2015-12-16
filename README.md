[![Stories in Ready](https://badge.waffle.io/inaka/elvis.png?label=ready&title=Ready)](https://waffle.io/inaka/elvis)

![](http://www.reactiongifs.com/wp-content/uploads/2013/01/elvis-dance.gif)

# elvis

Erlang Style Reviewer Core Library

## Contact Us
For **questions** or **general comments** regarding the use of this library,
please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/elvis/issues/new) in this repo
(or a pull request :)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io).

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
     [#{dirs => ["src", "test"],
        filter => "*.erl",
        rules => [{elvis_style, line_length, #{limit => 80}},
                  {elvis_style, no_tabs},
                  {elvis_style, no_trailing_whitespace},
                  {elvis_style, macro_names},
                  {elvis_style, macro_module_names},
                  {elvis_style, operator_spaces, #{rules => [{right, ","},
                                                             {right, "++"},
                                                             {left, "++"}]}
                  }
                 ]
       },
      #{dirs => ["."],
        filter => "Makefile",
        rules => [{elvis_project, no_deps_master_erlang_mk, #{ignore => []}},
                  {elvis_project, protocol_for_deps_erlang_mk, #{ignore => []}}]
       },
      #{dirs => ["."],
        filter => "rebar.config",
        rules => [{elvis_project, no_deps_master_rebar, #{ignore => []}},
                  {elvis_project, protocol_for_deps_rebar, #{ignore => []}}]
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
files that match `filter`, which will be run through each of the rules
specified by the `rules` entry, which is a list of items with the following
structure `{Module, Function, RuleConfig}` or `{Module, Function}` if the rule
takes no configuration values.

The implementation of a rule is just a function that takes 3 arguments: `elvis`'s
`config` entry from its [configuration](#configuration); the file to be
analyzed; and a configuration map specified for the rule. This means you can
define rules of your own as long as the functions that implement them respect
this arity.

There's currently no default configuration for `elvis`, but in the meantime
you can take the one in `config/elvis.config` as a starting point.

The GitHub configuration parameters `github_user` and `github_password` are
required only when `elvis` is used as a [webhook](#webhook).

## Implemented Rules

A reference of all rules implemented in Elvis can be found in this wiki page:
[Rules](https://github.com/inaka/elvis/wiki/Rules).

## References

Inspired by [HoundCI][houndci]

  [houndci]: https://houndci.com/
  [config]: http://www.erlang.org/doc/man/config.html
