-module(pass_prefer_idiomatic_calls).

-export([
    alternatives/1,
    other_calls/3,
    other_arities/2,
    idiomatic_alternatives/2,
    iterators/3,
    library_functions/4
]).

alternatives(Map) ->
    _ = Map#{key => value},
    _ = Map#{key := value},
    _ = "left" =:= "right",
    _ = "left" ++ "right",
    _ = "text" =:= [],
    _ = <<"text">> =:= <<>>,
    [1] ++ [2].

other_calls(Module, Function, Map) ->
    %% Function references and dynamic calls are outside the fixed call list.
    _ = fun maps:put/3,
    _ = Module:append([1], [2]),
    _ = lists:Function([1], [2]),
    maps:map(fun(_Key, Value) -> Value * 2 end, Map).

other_arities(Left, Right) ->
    _ = lists:append([Left, Right]),
    _ = string:equal(Left, Right, true),
    string:equal(Left, Right, true, nfc).

idiomatic_alternatives(Map, List) ->
    #{key := Value} = Map,
    _ = Map#{key => Value},
    _ = List =:= [],
    _ =
        case List of
            [] -> empty;
            [_ | _] -> non_empty
        end,
    _ = <<"text">>,
    "text".

%% Comprehension-based rewrites are outside the scope of this rule.
iterators(Fun, Map, List) ->
    _ = lists:map(Fun, List),
    _ = lists:map(fun erlang:abs/1, List),
    _ = lists:map(fun(Element) -> Element + 1 end, List),
    _ = lists:filter(fun(Element) -> Element > 0 end, List),
    _ = maps:map(fun(_Key, Value) -> Value * 2 end, Map),
    _ = maps:from_list([{key, value}]),
    maps:to_list(Map).

%% Preferences for other APIs or options belong outside this style rule.
library_functions(Table, Map, List, Step) ->
    %% Atom creation is checked by no_common_caveats_call.
    _ = list_to_atom("text"),
    _ = binary_to_atom(<<"text">>),
    _ = binary_to_atom(<<"text">>, utf8),
    %% Converting an existing atom to a binary is allowed.
    _ = atom_to_binary(text),
    _ = atom_to_binary(text, utf8),
    _ = ets:lookup(Table, key),
    _ = lists:foldl(fun({Key, Value}, Acc) -> Acc#{Key => Value} end, Map, [{key, value}]),
    _ = lists:flatten(List),
    _ = lists:reverse(List),
    _ = hd(List),
    _ = lists:nth(1, List),
    _ = lists:zip([1], List),
    _ = lists:seq(1, 3),
    _ = lists:seq(1, 3, Step),
    _ = string:lowercase("TEXT"),
    _ = binary:encode_hex(<<"text">>),
    _ = sets:new(),
    sets:from_list(List).
