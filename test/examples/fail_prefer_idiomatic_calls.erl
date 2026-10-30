-module(fail_prefer_idiomatic_calls).

-export([fail/6, nested_calls/0, library_calls/3]).

%% The rule checks calls by module, function, and arity, regardless of arguments.
fail(Map, Table, Left, Right, Fun, List) ->
    _ = maps:put(key, value, Map),
    _ = maps:update(key, value, Map),
    _ = string:equal(Left, Right),
    _ = string:concat(Left, Right),
    _ = string:is_empty(Left),
    _ = lists:append(Left, Right),
    _ = ets:lookup(Table, key),
    lists:map(Fun, List).

nested_calls() ->
    _ = <<(length(lists:append([1], [2]))):8>>,
    case lists:append([1], [2]) of
        List -> List
    end.

library_calls(Map, List, Step) ->
    _ = maps:find(key, Map),
    _ = maps:merge(Map, #{key => value}),
    _ = maps:from_list([{key, value}]),
    _ = maps:to_list(Map),
    _ = lists:foldl(fun({Key, Value}, Acc) -> Acc#{Key => Value} end, #{}, [{key, value}]),
    _ = length(List),
    _ = lists:flatten(List),
    _ = lists:reverse(List),
    _ = hd(List),
    _ = lists:nth(1, List),
    _ = lists:zip([1], List),
    _ = lists:seq(1, 3),
    _ = lists:seq(1, 3, Step),
    _ = string:lowercase("TEXT"),
    _ = binary:encode_hex(<<"text">>),
    _ = list_to_binary("text"),
    _ = list_to_atom("text"),
    _ = atom_to_list(text),
    _ = atom_to_binary(text),
    _ = sets:new(),
    sets:from_list(List).
