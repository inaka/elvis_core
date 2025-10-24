-module(pass_strict_module_layout).

-include("./prefer_include.hrl").

-dialyzer(no_match).
-elvis([{elvis_style, no_macros, #{allow => ['__']}}]).
-mixin([]).

-type my_type() :: ok.
-ifdef(TEST).
-type name() :: definition.
-endif.

-export_type([my_type/0, my_type_2/0]).
-ifdef(TEST).
-export_type([name/0]).
-endif.

-export([some_fun/0, some_fun_2/0, some_fun_3/0, some_fun_4/0]).

% -ifdef(TEST).
% -type name() :: definition.
% -export_type([name/0]).
% -endif.

-spec some_fun() -> my_type().
some_fun() -> ok.

-spec some_fun_2() -> my_type().
some_fun_2() -> ok.

-spec some_fun_3() -> my_type().
some_fun_3() -> ok.

-type my_type_2() :: ok.

-spec some_fun_4() -> my_type().
some_fun_4() -> ok.
