-module(fail_no_call_classes).
-export([fail/0, nested_calls/1]).

-spec fail() -> any().
fail() ->
    _ = timer:send_after(0, fail_after),
    _ = timer:send_after(0, self(), fail_after),

    _ = timer:send_interval(0, fail_interval),
    _ = timer:send_interval(0, self(), fail_interval),

    _ = size({1,2,3}),
    _ = size(<<"fail_size">>),

    _ = erlang:tuple_size({1,2,3}),
    _ = erlang:byte_size(<<"ok_size">>),

    _ = gen_server:call(self(), request),
    _ = gen_server:call(self(), request, infinity),

    _ = gen_event:call(self(), self(), request),
    _ = gen_event:call(self(), self(), request, infinity),

    _ = gen_statem:call(self(), request),
    _ = gen_statem:call(self(), request, infinity),

    _ = list_to_atom("heya"),
    _ = binary_to_atom(<<"heya">>),
    _ = binary_to_atom(<<"heya">>, utf8),
    _ = erlang:garbage_collect().

%% Calls in guards and binary segments must be found; function references are allowed.
nested_calls(Value) when size(Value) > 0 ->
    _ = fun erlang:size/1,
    _ = <<(size(Value)):8>>,
    _ = <<0:(size(Value))>>,
    %% The case expression is reachable through both content and node_attrs.
    case size(Value) of
        Size -> Size
    end.
