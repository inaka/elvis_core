-module(fail_abc_size).

-export([big_function/1]).

%% ABC breakdown:
%% A (assignments/matches): X1=, X2=, X3=, X4=, X5=, X6=, X7=, X8= -> 8
%% B (branches/calls): lists:seq, lists:map, erlang:+, lists:filter,
%%                     erlang:>, lists:sum, lists:reverse, io:format -> 8
%% C (conditions): case 3 clauses -> 2, andalso -> 1, orelse -> 1, > -> 1, < -> 1 -> 6
%% Magnitude: sqrt(64 + 64 + 36) = sqrt(164) ~= 12.8
big_function(Input) ->
    X1 = lists:seq(1, Input),
    X2 = lists:map(fun(X) -> X + 1 end, X1),
    X3 = lists:filter(fun(X) -> X > 5 end, X2),
    X4 = lists:sum(X3),
    X5 = lists:reverse(X2),
    X6 = case X4 of
        N when N > 100 ->
            io:format("big: ~p~n", [N]),
            X7 = N * 2,
            X7;
        N when 0 < N andalso N < 50 ->
            X8 = N + 1,
            X8;
        _ ->
            (X4 > 10) orelse (X4 < 0) orelse 0
    end,
    {X5, X6}.
