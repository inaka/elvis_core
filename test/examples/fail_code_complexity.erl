-module(fail_code_complexity).

-export([complex_function/2]).

%% Cyclomatic complexity breakdown:
%% Base: 1
%% 2 function clauses -> +1
%% case with 3 clauses -> +2
%% if with 2 clauses -> +1
%% andalso -> +1
%% orelse -> +1
%% receive with 2 clauses -> +1
%% Total: 8
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
