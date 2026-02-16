-module(pass_strict_module_layout).
-moduledoc("").

-vsn(1.0).

-include("./prefer_include.hrl").

-dialyzer(no_match).
-elvis([{elvis_style, no_macros, disable}]).
-mixin([]).
-hank([]).
-import(test_module, []).

-behaviour(gen_event).

-type my_type() :: ok.
-opaque my_opaque() :: definition.

-ifdef(TEST).
-type name() :: definition.
-export_type([name/0]).
-endif.

-if(ok).
-type apple() :: apple.
-export_type([apple/0]).
-elif(notok).
-type orange() :: orange.
-export_type([orange/0]).
-else.
-type banana() :: banana.
-export_type([banana/0]).
-endif.

-export_type([my_opaque/0]).
-export([some_fun/0, some_fun_2/0, some_fun_3/0, some_fun_4/0]).
-export([init/1, handle_call/2, handle_event/2]).


-define(MACRO, ok).

-doc("doc").

-spec some_fun() -> my_type().
some_fun() -> ?MACRO.

-spec some_fun_2() -> my_type().
some_fun_2() -> ok.

-spec some_fun_3() -> my_type().
some_fun_3() -> ok.

-spec some_fun_4() -> my_type().
some_fun_4() -> ok.

handle_call(_, _) -> ok.
handle_event(_, _) -> ok.
init(_) -> ok.
