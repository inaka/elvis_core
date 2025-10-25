-module(pass_strict_module_layout).

-include("./prefer_include.hrl").

-dialyzer(no_match).
-elvis([{elvis_style, no_macros, disable}]).
-mixin([]).
-hank([]).
-import(test_module, []).

-type my_type() :: ok.
-opaque my_opaque() :: definition.

-ifdef(TEST).
-type name() :: definition.
-export_type([name/0]).
-endif.

-export_type([my_opaque/0]).
-export([some_fun/0, some_fun_2/0, some_fun_3/0, some_fun_4/0]).

-define(MACRO, ok).

-spec some_fun() -> my_type().
some_fun() -> ?MACRO.

-spec some_fun_2() -> my_type().
some_fun_2() -> ok.

-spec some_fun_3() -> my_type().
some_fun_3() -> ok.

-spec some_fun_4() -> my_type().
some_fun_4() -> ok.
