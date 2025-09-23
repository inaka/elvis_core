-module(elvis_results_new_item).
-behaviour(gen_server).

-export([handle_call/3]).
-export([handle_cast/2]).
-export([init/1]).

-export([without_line/0]).
-export([without_column/0]).
-export([wihout_limit/1]).
-export([without_data/0]).
-export([with_attrs_for_data/0]).
-export([complete/1]).

-record(rec, {}).

without_line() ->
    ok.

without_column() ->
    ok.


wihout_limit(A) ->
    if A =:= 0 -> true;
        true -> false % Confusing, right?
    end.

without_data() ->
    ok.

with_attrs_for_data() ->
    ok.

-spec complete(#rec{}) -> #rec{}.
complete(A) ->
    A.

handle_call(_, _, _) ->
    {error, not_implemented}.

handle_cast(_, _) ->
    {error, not_implemented}.

init(_) ->
    R = fun (_, _) -> {error, not_implemented} end,
    R(nil, nil).
