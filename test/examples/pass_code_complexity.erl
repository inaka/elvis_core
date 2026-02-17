-module(pass_code_complexity).

-feature(maybe_expr, enable).

-export([simple_function/1, another_simple/1,
         with_maybe/1, with_try_catch_after/1, with_old_catch/1,
         with_lc/1, with_mc/1, with_bc/1, with_anon_fun/0]).

%% Complexity: 1 (base) + 2 (case clauses - 1) = 3
simple_function(X) ->
    case X of
        a -> 1;
        b -> 2;
        _ -> 3
    end.

%% Complexity: 1
another_simple(X) when is_integer(X) ->
    X + 1.

%% maybe: else clauses contribute decision points (clauses - 1).
%% Complexity: 1 (base) + 1 (2 else clauses - 1) = 2
with_maybe(X) ->
    maybe
        {ok, Y} ?= X,
        Y
    else
        {error, _} -> error;
        _ -> unknown
    end.

%% try/catch/after: catch clauses contribute via try_catch (clauses - 1).
%% The after block does NOT contribute to complexity.
%% Complexity: 1 (base) + 1 (2 catch clauses - 1) = 2
with_try_catch_after(X) ->
    try
        X
    catch
        error:_ -> 0;
        throw:_ -> 0
    after
        ok
    end.

%% old-style catch: does NOT contribute to cyclomatic complexity.
%% Complexity: 1 (base)
with_old_catch(X) ->
    catch X.

%% List comprehension: the LC itself doesn't add complexity,
%% but andalso/orelse inside it would count.
%% Complexity: 1 (base)
with_lc(L) ->
    [X || X <- L].

%% Map comprehension: same as list comprehension.
%% Complexity: 1 (base)
with_mc(M) ->
    #{K => V || K := V <- M}.

%% Binary comprehension: same as list/map comprehension.
%% Complexity: 1 (base)
with_bc(B) ->
    << <<X>> || <<X>> <= B>>.

%% Anonymous functions: their bodies are NOT counted towards the
%% enclosing function's complexity (skipped entirely).
%% Complexity: 1 (base)
with_anon_fun() ->
    F = fun(X) ->
        case X of
            1 -> one;
            2 -> two;
            _ -> other
        end
    end,
    F.
