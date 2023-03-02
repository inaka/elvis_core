-module(right_param_pattern_matching).

-export([single/1, multiple/1, different_param/3, different_clause/1]).
-export([regular_matching_works_fine/0]).

single({right, side, assignment} = Simple) ->
    fun({right, side, assignment} = SimpleToo) -> Simple == SimpleToo end.

%% This is not a match. According to Erlang precedence rules, the following code is actually
%% equivalent to: {on, the, right, side} = (Multiple = Assignments).
%% It's a tuple against a match, not a tuple against a variable against another variable
%% That's why no warnings are emitted for this line even if side is 'left'.
multiple({on, the, right, side} = Multiple = Assignments) ->
    fun({on, the, right, side} = MultipleToo = AssignmentsToo) ->
       {Multiple, Assignments} == {MultipleToo, AssignmentsToo}
    end.

different_param(happens_on, #{the_second := param_of} = The, function) ->
    fun(happens_on, #{the_second := param_of} = TheToo, function) -> The == TheToo
    end.

different_clause("it doesn't happen on the first clause") ->
    not_here;
different_clause("it does in the second one" = AsYoda) ->
    fun ("it doesn't happen on the first clause") ->
            not_here;
        ("it does in the second one" = AsYodaToo) ->
            AsYoda == AsYodaToo
    end.

regular_matching_works_fine() ->
    This = works:fine(),
    case This of
        This = #{is := also} ->
            valid;
        _ ->
            fun() ->
                Like = This,
                valid:as_well(Like)
            end
    end.
