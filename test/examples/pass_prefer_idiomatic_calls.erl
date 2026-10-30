-module(pass_prefer_idiomatic_calls).

-export([alternatives/2, other_calls/3, other_arities/2, idiomatic_alternatives/2]).

alternatives(Map, Table) ->
    _ = Map#{key => value},
    _ = Map#{key := value},
    _ = "left" =:= "right",
    _ = "left" ++ "right",
    _ = "text" =:= [],
    _ = <<"text">> =:= <<>>,
    _ = [1] ++ [2],
    _ = ets:lookup_element(Table, key, 2),
    [Element + 1 || Element <- [1, 2, 3]].

other_calls(Module, Function, Map) ->
    %% Function references and dynamic calls are outside the fixed call list.
    _ = fun lists:map/2,
    _ = Module:append([1], [2]),
    _ = lists:Function([1], [2]),
    maps:map(fun(_Key, Value) -> Value * 2 end, Map).

other_arities(Left, Right) ->
    _ = lists:append([Left, Right]),
    _ = string:equal(Left, Right, true),
    string:equal(Left, Right, true, nfc).

idiomatic_alternatives(Map, List) ->
    _ = #{Key => Value || Key := Value <- Map},
    _ = [Value || _Key := Value <- Map],
    _ = lists:flatlength(List),
    _ = lists:reverse(List, [tail]),
    _ = lists:last(List),
    _ = lists:enumerate(List),
    _ = lists:enumerate(0, List),
    _ = binary:encode_hex(<<"text">>, lowercase),
    _ = atom_to_binary(text, utf8),
    _ = sets:new([{version, 2}]),
    sets:from_list(List, [{version, 2}]).
