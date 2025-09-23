-module(fail_guard_operators).

-export([
    case_clauses/0,
    else_clauses/0,
    function_clauses_first/1,
    function_clauses_second/1,
    function_clauses_third/1,
    fun_clauses/0,
    if_clauses/3,
    receive_clauses/0,
    try_clauses/0,
    catch_clauses/0,
    with_not/0
]).

case_clauses() ->
    {
        case first:expression() of
            X when X == a; X =< 10 -> {clause, 1};
            X when X == b orelse X == 10 -> {clause, 2};
            X when (X == c orelse X == d); X >= 10 -> {clause, 3}
        end,
        case second:expression() of
            Y when Y == a; Y =< 10 -> {clause, 1};
            Y when Y == b; Y == 10 -> {clause, 2};
            Y when Y == c; Y == d; Y >= 10 -> {clause, 3}
        end,
        case third:expression() of
            Z when Z == a andalso Z =< 10 -> {clause, 1};
            Z when Z == b andalso Z == 10 -> {clause, 2};
            Z when Z == c andalso Z == d andalso Z >= 10 -> {clause, 3}
        end
    }.

else_clauses() ->
    {
        try
            first:expression()
        catch
            X when X == a; X =< 10 -> {clause, 1};
            X when X == b orelse X == 10 -> {clause, 2};
            X when (X == c orelse X == d); X >= 10 -> {clause, 3}
        end,
        try
            second:expression()
        catch
            Y when Y == a; Y =< 10 -> {clause, 1};
            Y when Y == b; Y == 10 -> {clause, 2};
            Y when Y == c; Y == d; Y >= 10 -> {clause, 3}
        end,
        try
            third:expression()
        catch
            Z when Z == a or (Z =< 10) -> {clause, 1};
            Z when Z == b or (Z == 10) -> {clause, 2};
            Z when Z == c or (Z == d) or (Z >= 10) -> {clause, 3}
        end
    }.

function_clauses_first(X) when X == a; X =< 10 -> {clause, 1};
function_clauses_first(X) when X == b orelse X == 10 -> {clause, 2};
function_clauses_first(X) when (X == c orelse X == d); X >= 10 -> {clause, 3}.
function_clauses_second(Y) when Y == a; Y =< 10 -> {clause, 1};
function_clauses_second(Y) when Y == b; Y == 10 -> {clause, 2};
function_clauses_second(Y) when Y == c; Y == d; Y >= 10 -> {clause, 3}.
function_clauses_third(Z) when Z == a and (Z =< 10) -> {clause, 1};
function_clauses_third(Z) when Z == b and (Z == 10) -> {clause, 2};
function_clauses_third(Z) when Z == c and (Z == d) and (Z >= 10) -> {clause, 3}.

fun_clauses() ->
    {
        fun
            (X) when X == a; X =< 10 -> {clause, 1};
            (X) when X == b orelse X == 10 -> {clause, 2};
            (X) when (X == c orelse X == d); X >= 10 -> {clause, 3}
        end,
        fun
            (Y) when Y == a; Y =< 10 -> {clause, 1};
            (Y) when Y == b; Y == 10 -> {clause, 2};
            (Y) when Y == c; Y == d; Y >= 10 -> {clause, 3}
        end,
        fun
            Third(Z) when Z == a orelse Z =< 10 -> Third(1);
            Third(Z) when Z == b orelse Z == 10 -> Third(2);
            Third(Z) when Z == c orelse Z == d orelse Z >= 10 -> Third(3)
        end
    }.

if_clauses(X, Y, Z) ->
    {
        if
            X == a; X =< 10 -> {clause, 1};
            X == b orelse X == 10 -> {clause, 2};
            (X == c orelse X == d); X >= 10 -> {clause, 3}
        end,
        if
            Y == a; Y =< 10 -> {clause, 1};
            Y == b; Y == 10 -> {clause, 2};
            Y == c; Y == d; Y >= 10 -> {clause, 3}
        end,
        if
            Z == a orelse Z =< 10 -> {clause, 1};
            Z == b orelse Z == 10 -> {clause, 2};
            Z == c orelse Z == d orelse Z >= 10 -> {clause, 3}
        end
    }.

receive_clauses() ->
    {
        receive
            X when X == a; X =< 10 -> {clause, 1};
            X when X == b orelse X == 10 -> {clause, 2};
            X when (X == c orelse X == d); X >= 10 -> {clause, 3}
        end,
        receive
            Y when Y == a; Y =< 10 -> {clause, 1};
            Y when Y == b; Y == 10 -> {clause, 2};
            Y when Y == c; Y == d; Y >= 10 -> {clause, 3}
        end,
        receive
            Z when Z == a orelse Z =< 10 -> {clause, 1};
            Z when Z == b orelse Z == 10 -> {clause, 2};
            Z when Z == c orelse Z == d orelse Z >= 10 -> {clause, 3}
        end
    }.

try_clauses() ->
    {
        try first:expression() of
            X when X == a; X =< 10 -> {clause, 1};
            X when X == b orelse X == 10 -> {clause, 2};
            X when (X == c orelse X == d); X >= 10 -> {clause, 3}
        catch
            _:_ -> error
        end,
        try second:expression() of
            Y when Y == a; Y =< 10 -> {clause, 1};
            Y when Y == b; Y == 10 -> {clause, 2};
            Y when Y == c; Y == d; Y >= 10 -> {clause, 3}
        catch
            _:_ -> error
        end,
        try third:expression() of
            Z when Z == a orelse Z =< 10 -> {clause, 1};
            Z when Z == b orelse Z == 10 -> {clause, 2};
            Z when Z == c orelse Z == d orelse Z >= 10 -> {clause, 3}
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
            _:X when X == b orelse X == 10 -> {clause, 2};
            _:X when (X == c orelse X == d); X >= 10 -> {clause, 3}
        end,
        try
            second:expression()
        catch
            _:Y when Y == a; Y =< 10 -> {clause, 1};
            _:Y when Y == b; Y == 10 -> {clause, 2};
            _:Y when Y == c; Y == d; Y >= 10 -> {clause, 3}
        end,
        try
            third:expression()
        catch
            _:Z when Z == a orelse Z =< 10 -> {clause, 1};
            _:Z when Z == b orelse Z == 10 -> {clause, 2};
            _:Z when Z == c orelse Z == d orelse Z >= 10 -> {clause, 3}
        end
    }.

with_not() ->
    {
        case first:expression() of
            X when not X == a; not X =< 10 -> {clause, 1};
            X when not not (not X == b orelse not X == 10) -> {clause, 2};
            X when not not not (X == c orelse X == d); not X >= 10 -> {clause, 3}
        end,
        case second:expression() of
            Y when not Y == a; not Y =< 10 -> {clause, 1};
            Y when not Y == b; not not Y == 10 -> {clause, 2};
            Y when not not not Y == c; not not not Y == d; not not not Y >= 10 -> {clause, 3}
        end,
        case third:expression() of
            Z when not (Z == a andalso Z =< 10) -> {clause, 1};
            Z when not not (not Z == b andalso not Z == 10) -> {clause, 2};
            Z when not not not (Z == c andalso Z == d andalso Z >= 10) -> {clause, 3}
        end
    }.
