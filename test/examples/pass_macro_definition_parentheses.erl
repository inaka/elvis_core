-module(pass_macro_definition_parentheses).

-export([get_attr/1, x/0]).

-define(CONSTANT, test).
-define(CONSTANTaa, 5).
-define(GET_NAME(), get_attr(name)).
-define(GET_AGE(), get_attr(age)).
-define(GET_ACTIONS(), actions_module:get_actions()).
-define(SOCKET_ERROR(State),
    ((Error =:= tcp_error andalso element(2, State) =:= tcp) orelse
        (Error =:= ssl_error andalso element(2, State) =:= ssl))
).

-define(SOMETHING(), something:poorly(written).

-define(THE_MACRO, io:format("~p\n", [).

x() ->
    ?THE_MACRO
    bananas]).

get_attr(name) -> "John";
get_attr(age) -> 25.
