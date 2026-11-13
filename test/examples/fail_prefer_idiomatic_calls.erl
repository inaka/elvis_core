-module(fail_prefer_idiomatic_calls).

-export([fail/3, nested_calls/0, library_calls/2]).

%% These calls have alternatives expressed with Erlang syntax or literals.
fail(Map, Left, Right) ->
    _ = maps:put(key, value, Map),
    _ = maps:update(key, value, Map),
    _ = string:equal(Left, Right),
    _ = string:concat(Left, Right),
    _ = string:is_empty(Left),
    lists:append(Left, Right).

nested_calls() ->
    _ = <<
        (case length(lists:append([1], [2])) of
            0 -> 0;
            _ -> 1
        end):8
    >>,
    case lists:append([1], [2]) of
        List -> List
    end.

library_calls(Map, List) ->
    {ok, Value} = maps:find(key, Map),
    _ = maps:merge(Map, #{key => Value}),
    _ = length(List) =:= 0,
    _ = list_to_binary("text"),
    atom_to_list(text).
