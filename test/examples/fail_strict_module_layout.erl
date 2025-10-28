-module(fail_strict_module_layout).

-elvis([{elvis_style, no_macros, #{allow => ['__']}}]).

-export_type([my_type/0]).

-export([some_fun/0]).
-import(test_module, []).

-behaviour(gen_event).
-export([init/1, handle_event/2]).

-type my_type() :: ok.
-export([handle_call/2]).

-mixin([]).
-doc("doc").
-vsn(1.0).
-moduledoc("").

-spec some_fun() -> my_type().
some_fun() -> ok.

-dialyzer({nowarn_function, []}).

-compile("").

handle_call(_, _) -> ok.
handle_event(_, _) -> ok.
init(_) -> ok.

