-module(pass_prefer_strict_generators).

-if(?OTP_RELEASE >= 28).

-export([bitstrings/1, lists/1, maps/1, edge_cases/0]).

bitstrings(Input) ->
    {
        <<<<X>> || <<X:8>> <:= Input>>,
        [X || <<X:4/little-signed-integer-unit:8>> <:= Input],
        #{X => Y || <<X:8>> <:= Input, <<Y:1>> <:= X}
    }.

lists(Input) ->
    {
        <<<<X>> || X <:- Input>>,
        [X || {X, _} <:- Input],
        #{X => Y || X <:- Input, Y <:- X}
    }.

maps(Input) ->
    {
        <<<<X/binary, Y/binary>> || X := <<Y/binary>> <:- Input>>,
        [{X, Y} || X := <<Y/binary>> <:- Input],
        #{X => Y || X := Ys <:- Input, _ := Y <:- Ys}
    }.

edge_cases() ->
    #{
        no_generator => [x || true] ++ [y || a:something() > an:other(), yet:another(filter)],
        filter_before_generator => <<<<X:8>> || a:filter(), <<X:4>> <:= a:generator()>>,
        nested =>
            #{
                [A || A <:- Keys] => <<<<Value:1>> || Value <:- Values>>
             || Keys := Values <:- key:values()
            }
    }.

-endif.
