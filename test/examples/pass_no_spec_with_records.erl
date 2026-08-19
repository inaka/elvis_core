-module(pass_no_spec_with_records).

-dialyzer(no_contracts).

-export([
         function_1/1,
         function_2/2,
         function_3/2
        ]).

-record(state, {}).

-if(?OTP_RELEASE >= 29).
-record #native{}.
-else.
-record(native, {}).
-endif.

-type state() :: #state{}.
-type native() :: #native{}.
-export_type([state/0, native/0]).

-spec function_1(atom()) -> atom().
function_1(Arg) ->
    Arg.

-spec function_2(atom(), atom()) -> state().
function_2(_Arg1, _Arg2) ->
    #state{}.

-spec function_3(atom(), ok | error) -> native().
function_3(_Arg1, _Arg2) ->
    #native{}.
