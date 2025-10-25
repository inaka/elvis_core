-module(fail_strict_module_layout).

-elvis([{elvis_style, no_macros, #{allow => ['__']}}]).

-export_type([my_type/0]).

-export([some_fun/0]).

-type my_type() :: ok.

-mixin([]).

-spec some_fun() -> my_type().
some_fun() -> ok.

-dialyzer({nowarn_function, []}).

