-module(pass_guard_operators).

-feature(maybe_expr, enable).

-export([
    case_clauses/0,
    else_clauses/0,
    function_clauses_first/1,
    function_clauses_second/1,
    fun_clauses/0,
    if_clauses/2,
    receive_clauses/0,
    try_clauses/0,
    catch_clauses/0
]).

case_clauses() ->
    {
        case first:expression() of
            X when X == a; X =< 10 -> {clause, 1};
            X when X == b; X == 10 -> {clause, 2};
            X when X == c; X == d; X >= 10 -> {clause, 3}
        end,
        case second:expression() of
            Y when Y == a orelse Y =< 10 -> {clause, 1};
            Y when Y == b orelse Y == 10 -> {clause, 2};
            Y when Y == c orelse Y == d orelse Y >= 10 -> {clause, 3}
        end
    }.

else_clauses() ->
    {
        maybe
            first:expression()
        else
            X when X == a; X =< 10 -> {clause, 1};
            X when X == b; X == 10 -> {clause, 2};
            X when X == c; X == d; X >= 10 -> {clause, 3}
        end,
        maybe
            second:expression()
        else
            Y when Y == a orelse Y =< 10 -> {clause, 1};
            Y when Y == b orelse Y == 10 -> {clause, 2};
            Y when Y == c orelse Y == d orelse Y >= 10 -> {clause, 3}
        end
    }.

function_clauses_first(X) when X == a; X =< 10 -> {clause, 1};
function_clauses_first(X) when X == b; X == 10 -> {clause, 2};
function_clauses_first(X) when X == c; X == d; X >= 10 -> {clause, 3}.
function_clauses_second(Y) when Y == a orelse Y =< 10 -> {clause, 1};
function_clauses_second(Y) when Y == b orelse Y == 10 -> {clause, 2};
function_clauses_second(Y) when Y == c orelse Y == d orelse Y >= 10 -> {clause, 3}.

fun_clauses() ->
    {
        fun
            (X) when X == a; X =< 10 -> {clause, 1};
            (X) when X == b; X == 10 -> {clause, 2};
            (X) when X == c; X == d; X >= 10 -> {clause, 3}
        end,
        fun
            Second(Y) when Y == a orelse Y =< 10 -> Second(1);
            Second(Y) when Y == b orelse Y == 10 -> Second(2);
            Second(Y) when Y == c orelse Y == d orelse Y >= 10 -> Second(3)
        end
    }.

if_clauses(X, Y) ->
    {
        if
            X == a; X =< 10 -> {clause, 1};
            X == b; X == 10 -> {clause, 2};
            X == c; X == d; X >= 10 -> {clause, 3}
        end,
        if
            Y == a orelse Y =< 10 -> {clause, 1};
            Y == b orelse Y == 10 -> {clause, 2};
            Y == c orelse Y == d orelse Y >= 10 -> {clause, 3}
        end
    }.

receive_clauses() ->
    {
        receive
            X when X == a; X =< 10 -> {clause, 1};
            X when X == b; X == 10 -> {clause, 2};
            X when X == c; X == d; X >= 10 -> {clause, 3}
        end,
        receive
            Y when Y == a orelse Y =< 10 -> {clause, 1};
            Y when Y == b orelse Y == 10 -> {clause, 2};
            Y when Y == c orelse Y == d orelse Y >= 10 -> {clause, 3}
        end
    }.

try_clauses() ->
    {
        try first:expression() of
            X when X == a; X =< 10 -> {clause, 1};
            X when X == b; X == 10 -> {clause, 2};
            X when X == c; X == d; X >= 10 -> {clause, 3}
        catch
            _:_ -> error
        end,
        try second:expression() of
            Y when Y == a orelse Y =< 10 -> {clause, 1};
            Y when Y == b orelse Y == 10 -> {clause, 2};
            Y when Y == c orelse Y == d orelse Y >= 10 -> {clause, 3}
        catch
            _:_ -> error
        end
    }.

catch_clauses() ->
    {
        try
            first:expression()
        catch
            _:X when X == a; X =< 10 -> {clause, 1};
            _:X when X == b; X == 10 -> {clause, 2};
            _:X when X == c; X == d; X >= 10 -> {clause, 3}
        end,
        try
            second:expression()
        catch
            _:Y when Y == a orelse Y =< 10 -> {clause, 1};
            _:Y when Y == b orelse Y == 10 -> {clause, 2};
            _:Y when Y == c orelse Y == d orelse Y >= 10 -> {clause, 3}
        end
    }.
