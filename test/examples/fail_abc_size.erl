-module(fail_abc_size).

-feature(maybe_expr, enable).

-export([big_function/1,
         with_maybe/1, with_old_catch/1, with_receive/0,
         with_try_catch/1, with_lc/1, with_mc/1, with_bc/1,
         with_anon_fun/0]).

%% ABC breakdown:
%% A (assignments): X1=..X8= -> 8
%% B (branches/calls): lists:seq, lists:map, lists:filter,
%%                     lists:sum, lists:reverse, io:format -> 6
%% C (conditions): orelse x2, > x2, < x2 -> 6
%% Note: case clauses do NOT contribute to C (wrapped in case_clauses node).
%%       Guards (when ...) are NOT traversed, so ops in guards don't count.
%% Magnitude: sqrt(64 + 36 + 36) = sqrt(136) ~= 11.7
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

%% maybe: else clauses do NOT contribute to C
%% (they are wrapped in an 'else' node, not direct clause children of 'maybe').
%% ?= is a maybe_match, NOT a regular match, so it does NOT count as A.
%% A=4 (R1=..R4=), B=4 (is_list, length, hd, abs), C=2 (andalso, >)
%% Magnitude: sqrt(16 + 16 + 4) = sqrt(36) = 6.0
with_maybe(X) ->
    R1 = erlang:is_list(X),
    R2 = erlang:length(X),
    R3 = erlang:hd(X),
    maybe
        {ok, Y} ?= R3,
        R4 = erlang:abs(Y),
        R4
    else
        {error, bad} -> R1 andalso R2 > 0;
        {error, _} -> error;
        _ -> unknown
    end.

%% old-style catch: does NOT contribute to A, B, or C directly.
%% The ABC size here comes from the matches, calls, and conditions around it.
%% A=4 (R1=..R4=), B=4 (sort, sum, reverse, last), C=1 (>)
%% Note: case clauses don't contribute to C.
%% Magnitude: sqrt(16 + 16 + 1) = sqrt(33) ~= 5.7
with_old_catch(X) ->
    R1 = lists:sort(X),
    R2 = lists:sum(X),
    R3 = catch lists:reverse(X),
    R4 = lists:last(X),
    case R2 > R4 of
        true -> {R1, R3};
        false -> R3
    end.

%% receive: clauses contribute to C via receive_case (clauses - 1).
%% A=3 (R1=..R3=), B=3 (self, is_pid, node), C=3 (receive_case 4-1)
%% Note: ops in guards (V > 0) are NOT counted.
%% Magnitude: sqrt(9 + 9 + 9) = sqrt(27) ~= 5.2
with_receive() ->
    R1 = erlang:self(),
    R2 = erlang:is_pid(R1),
    R3 = erlang:node(),
    receive
        {stop, Reason} -> {R2, Reason};
        {data, V} when V > 0 -> {R3, V};
        go -> R1;
        _ -> error
    after
        1000 -> timeout
    end.

%% try/catch/after: catch clauses contribute to C via try_catch (clauses - 1).
%% The after block does NOT contribute to conditions.
%% A=3 (R1=..R3=), B=3 (sort, sum, max), C=3 (try_catch 4-1)
%% Note: ops in guards (V > 0) are NOT counted.
%% Magnitude: sqrt(9 + 9 + 9) = sqrt(27) ~= 5.2
with_try_catch(X) ->
    R1 = lists:sort(X),
    R2 = lists:sum(X),
    try
        R3 = lists:max(X),
        R3 + R2
    catch
        error:badarg -> {error, R1};
        error:_ -> {error, unknown};
        throw:V when V > 0 -> V;
        _:_ -> R2
    after
        ok
    end.

%% List comprehension: the LC itself doesn't add to ABC,
%% but calls/matches/ops inside it do count normally.
%% A=4 (R1=..R4=), B=4 (seq, sum, reverse, length), C=2 (> x2)
%% Note: case clauses don't contribute to C.
%% Magnitude: sqrt(16 + 16 + 4) = sqrt(36) = 6.0
with_lc(L) ->
    R1 = lists:seq(1, 10),
    R2 = [X + 1 || X <- L, X > 0],
    R3 = lists:sum(R2),
    R4 = lists:reverse(R1),
    case R3 > length(R4) of
        true -> R2;
        false -> R4
    end.

%% Map comprehension: same as list comprehension.
%% A=4 (R1=..R4=), B=4 (size, keys, sort, length), C=2 (> x2)
%% Magnitude: sqrt(16 + 16 + 4) = sqrt(36) = 6.0
with_mc(M) ->
    R1 = maps:size(M),
    R2 = #{K => V * 2 || K := V <- M, V > 0},
    R3 = maps:keys(R2),
    R4 = lists:sort(R3),
    case R1 > length(R4) of
        true -> R2;
        false -> R4
    end.

%% Binary comprehension: same as list/map comprehension.
%% A=4 (R1=..R4=), B=3 (byte_size x2, iolist_to_binary),
%% C=4 (> x2, andalso, >)
%% Magnitude: sqrt(16 + 9 + 16) = sqrt(41) ~= 6.4
with_bc(B) ->
    R1 = byte_size(B),
    R2 = << <<(X + 1)>> || <<X>> <= B, X > 0 >>,
    R3 = byte_size(R2),
    R4 = erlang:iolist_to_binary([B, R2]),
    case R1 > R3 andalso R3 > 0 of
        true -> R4;
        false -> R2
    end.

%% Anonymous functions: their bodies are NOT counted towards the
%% enclosing function's ABC size (skipped entirely).
%% The ABC size here comes from the code OUTSIDE the anonymous function.
%% A=5 (F=, R1=..R4=), B=4 (seq, map, sum, reverse), C=1 (>)
%% Magnitude: sqrt(25 + 16 + 1) = sqrt(42) ~= 6.5
with_anon_fun() ->
    F = fun(X) -> X * X + X + 1 end,
    R1 = lists:seq(1, 10),
    R2 = lists:map(F, R1),
    R3 = lists:sum(R2),
    R4 = lists:reverse(R2),
    case R3 > 100 of
        true -> R4;
        false -> R2
    end.
