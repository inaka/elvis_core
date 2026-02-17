-module(fail_code_complexity).

-feature(maybe_expr, enable).

-export([complex_function/2,
         with_maybe/1, with_try_catch_after/1, with_old_catch/1,
         with_lc/1, with_mc/1, with_bc/1, with_anon_fun/0]).

%% Cyclomatic complexity breakdown:
%% Base: 1
%% 2 function clauses -> +1
%% if with 2 clauses -> +1
%% andalso -> +1
%% orelse -> +1
%% receive_case with 2 clauses -> +1
%% Total: 6
%% Note: case clauses do NOT contribute (wrapped in case_clauses node).
complex_function(a, X) ->
    case X of
        1 ->
            if
                X > 0 -> ok;
                true -> error
            end;
        2 ->
            (X > 0 andalso X < 100) orelse (X =:= -1);
        _ ->
            receive
                stop -> ok;
                continue -> go
            end
    end;
complex_function(b, _X) ->
    simple.

%% maybe: else clauses do NOT contribute to complexity
%% (they are wrapped in an 'else' node, not direct clause children of 'maybe').
%% Complexity: 1 (base) + 2 (if 3-1) + 1 (if 2-1) + 1 (andalso) + 1 (orelse) = 6
with_maybe(X) ->
    maybe
        {ok, Y} ?= X,
        R = if
            Y > 0 andalso Y < 100 -> small;
            Y >= 100 -> big;
            true -> negative
        end,
        if
            R =:= small orelse R =:= big -> {ok, Y, R};
            true -> {ok, Y, unknown}
        end
    else
        {error, _} -> error;
        _ -> unknown
    end.

%% try/catch/after: catch clauses contribute via try_catch (clauses - 1).
%% The after block does NOT contribute to complexity.
%% Complexity: 1 (base) + 4 (try_catch 5-1) + 1 (andalso) + 1 (orelse) = 7
with_try_catch_after(X) ->
    try
        (is_list(X) andalso length(X) > 0) orelse is_atom(X)
    catch
        error:badarg -> badarg;
        error:badarith -> badarith;
        error:function_clause -> bad_fun;
        throw:V -> V;
        _:_ -> unknown
    after
        ok
    end.

%% old-style catch: does NOT contribute to cyclomatic complexity.
%% The complexity here comes from if/andalso/orelse around it.
%% Complexity: 1 (base) + 2 (if 3-1) + 1 (if 2-1) + 1 (andalso) + 1 (orelse) = 6
with_old_catch(X) ->
    R = catch X,
    T = if
        is_integer(R) andalso R > 0 -> positive;
        is_integer(R) -> non_positive;
        true -> not_integer
    end,
    if
        T =:= positive orelse T =:= non_positive -> {number, R};
        true -> {other, R}
    end.

%% List comprehension: the LC itself doesn't add complexity.
%% Complexity: 1 (base) + 2 (if 3-1) + 1 (if 2-1) + 1 (andalso) + 1 (orelse) = 6
with_lc(L) ->
    R = [X * 2 || X <- L],
    T = if
        length(R) > 10 andalso hd(R) > 0 -> large_positive;
        length(R) > 0 -> small;
        true -> empty
    end,
    if
        T =:= large_positive orelse T =:= small -> {ok, R};
        true -> {empty, []}
    end.

%% Map comprehension: same as list comprehension.
%% Complexity: 1 (base) + 2 (if 3-1) + 1 (if 2-1) + 1 (andalso) + 1 (orelse) = 6
with_mc(M) ->
    R = #{K => V || K := V <- M},
    T = if
        is_map(R) andalso map_size(R) > 0 -> non_empty;
        is_map(R) -> empty_map;
        true -> other
    end,
    if
        T =:= non_empty orelse T =:= empty_map -> {ok, R};
        true -> {error, R}
    end.

%% Binary comprehension: same as list/map comprehension.
%% Complexity: 1 (base) + 2 (if 3-1) + 1 (if 2-1) + 1 (andalso) + 1 (orelse) = 6
with_bc(B) ->
    R = << <<X>> || <<X>> <= B>>,
    T = if
        is_binary(R) andalso byte_size(R) > 0 -> non_empty;
        is_binary(R) -> empty_bin;
        true -> other
    end,
    if
        T =:= non_empty orelse T =:= empty_bin -> {ok, R};
        true -> {error, R}
    end.

%% Anonymous functions: their bodies are NOT counted towards the
%% enclosing function's complexity (skipped entirely).
%% The complexity here comes from if/andalso/orelse OUTSIDE the fun.
%% Complexity: 1 (base) + 2 (if 3-1) + 1 (if 2-1) + 1 (andalso) + 1 (orelse) = 6
with_anon_fun() ->
    F = fun(X) ->
        case X of
            1 -> one;
            2 -> two;
            _ -> other
        end
    end,
    R = F(erlang:system_time()),
    T = if
        is_atom(R) andalso R =/= undefined -> valid;
        is_atom(R) -> undef;
        true -> invalid
    end,
    if
        T =:= valid orelse T =:= undef -> {ok, R};
        true -> {error, R}
    end.
