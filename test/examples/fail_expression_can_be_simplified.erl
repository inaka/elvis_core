-module(fail_expression_can_be_simplified).
-export([
    bad/1,
    in_map/1,
    in_case/1,
    with_guard/1,
    in_receive/1,
    in_misc/1,
    in_bits/1
]).

%% Single violation for filtered test (add_zero_right)
bad(X) ->
    _ = X + 0,
    ok.

%% Map: key and value contexts
in_map(X) ->
    _ = #{([] ++ X) => a, b => (X ++ [])},
    ok.

%% case expression context
in_case(X) ->
    _ = case ([] -- X) of [] -> ok end,
    _ = case (X -- []) of _ -> ok end,
    ok.

%% Guard (subtract_zero)
with_guard(X) when (X - 0) =:= X ->
    ok.

%% receive ... after ... (div_by_one)
in_receive(X) ->
    receive _ -> ok after (X div 1) -> timeout end,
    _ = X rem 1,
    ok.

%% Mixed: tuple, list, andalso, orelse, not (7 violations)
in_misc(X) ->
    _ = {0 - X},
    _ = [X * 1],
    _ = {1 * X},
    _ = true andalso X,
    _ = false orelse X,
    _ = not true,
    _ = not false,
    ok.

%% Bit syntax (3 violations)
in_bits(X) ->
    _ = <<(X band -1):8>>,
    _ = <<(X bor 0):8>>,
    _ = <<(X bxor 0):8>>,
    ok.
