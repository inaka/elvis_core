-module(fail_no_spec_with_records).

-dialyzer({nowarn_function, function_2/2}).

-export([
         function_1/1,
         function_2/2,
         function_3/2,
         function_4/2,
         function_5/1
        ]).

-record(state, {}).

-if(?OTP_RELEASE >= 29).
-record #native{}.
-else.
-record(native, {}).
-endif.

-spec function_1(atom()) -> atom().
function_1(Arg) ->
    Arg.

-spec function_2(atom(), atom()) -> #state{}.
function_2(_Arg1, _Arg2) ->
    #state{}.

-if(?OTP_RELEASE >= 29).
-spec function_3(atom(), #state{}) -> #fail_no_spec_with_records:native{}.
-else.
-spec function_3(atom(), #state{}) -> #native{}.
-endif.
function_3(_Arg1, _Arg2) ->
    #native{}.

-spec function_4(atom(), integer()) -> atom().
function_4(_Arg1, _Arg2) ->
    ok.

-spec function_5(#state{}) -> ok.
function_5(_Arg1) ->
    ok.
