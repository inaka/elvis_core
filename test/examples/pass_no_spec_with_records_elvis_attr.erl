-module(pass_no_spec_with_records_elvis_attr).

-elvis([{elvis_style, function_naming_convention, disable}]).

-dialyzer({nowarn_function, function_2/2}).

-elvis([{elvis_style, no_spec_with_records, disable}]).


-export([
         function_1/1,
         function_2/2,
         function_3/2,
         function_4/2,
         function_5/1
        ]).

-record(state, {}).

-spec function_1(atom()) -> atom().
function_1(Arg) ->
    Arg.

-spec function_2(atom(), atom()) -> #state{}.
function_2(_Arg1, _Arg2) ->
    ok.

-spec function_3(atom(), #state{}) -> atom().
function_3(_Arg1, _Arg2) ->
    ok.

-spec function_4(atom(), integer()) -> atom().
function_4(_Arg1, _Arg2) ->
    ok.

-spec function_5(#state{}) -> ok.
function_5(_Arg1) ->
    ok.
