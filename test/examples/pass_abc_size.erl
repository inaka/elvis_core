-module(pass_abc_size).

-feature(maybe_expr, enable).

-export([simple/1, add/2,
         with_maybe/1, with_old_catch/1, with_receive/0,
         with_try_catch/1, with_lc/1, with_mc/1, with_bc/1,
         with_anon_fun/0]).

simple(X) ->
    X + 1.

add(A, B) ->
    A + B.

%% maybe: else clauses contribute to C (clauses - 1).
%% ?= is a maybe_match, NOT a regular match, so it does NOT count as A.
with_maybe(X) ->
    maybe
        {ok, Y} ?= X,
        Y
    else
        {error, _} -> error;
        _ -> unknown
    end.

%% old-style catch: does NOT contribute to A, B, or C directly.
with_old_catch(X) ->
    catch X.

%% receive: clauses contribute to C (clauses - 1).
with_receive() ->
    receive
        stop -> ok;
        go -> go
    end.

%% try/catch/after: catch clauses contribute to C via try_catch (clauses - 1).
%% The after block does NOT contribute to conditions.
with_try_catch(X) ->
    try
        X
    catch
        error:_ -> 0;
        throw:_ -> 0
    after
        ok
    end.

%% List comprehension: the LC itself doesn't add to ABC,
%% but calls/matches/ops inside it do count normally.
with_lc(L) ->
    [X || X <- L].

%% Map comprehension: same as list comprehension.
with_mc(M) ->
    #{K => V || K := V <- M}.

%% Binary comprehension: same as list/map comprehension.
with_bc(B) ->
    << <<X>> || <<X>> <= B>>.

%% Anonymous functions: their bodies are NOT counted towards the
%% enclosing function's ABC size (skipped entirely).
with_anon_fun() ->
    F = fun(X) -> X * X + X + 1 end,
    F.
