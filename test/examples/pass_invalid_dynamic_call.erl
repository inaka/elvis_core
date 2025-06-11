-module(pass_invalid_dynamic_call).

-type q(X) :: queue:quueue(X).
-type t() :: ?MODULE:q(?MODULE).

-export([
         regular_call/1,
         erlang_apply/3,
         module_name/0
        ]).

-spec regular_call(Arg1) -> ?MODULE:q(Arg1).
regular_call(Argument) ->
    regular:call(),
    regular:call(with, Argument).

-spec erlang_apply(erlang:module(), atom(), any()) -> pass_invalid_dynamic_call:t().
erlang_apply(Module, Function, Args) ->
    erlang:apply(Module, Function, Args).

-spec module_name() -> ?MODULE:q(string:string()).
module_name() ->
    ?MODULE:regular_call("Using ?MODULE is valid").
