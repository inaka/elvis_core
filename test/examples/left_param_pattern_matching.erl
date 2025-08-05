-module(left_param_pattern_matching).

-export([single/1, multiple/1, different_param/3, different_clause/1]).

single(Simple = {left, side, assignment}) ->
    fun(SimpleToo = {left, side, assignment}) -> Simple == SimpleToo end.

multiple(Multiple = Assignments = {on, the, left, side}) ->
    fun(MultipleToo = AssignmentsToo = {on, the, left, side}) ->
       {Multiple, Assignments} == {MultipleToo, AssignmentsToo}
    end.

different_param(happens_on, TheSecond = #{param := of_the}, function) ->
    fun NamedFun(happens_on, TheSecondToo = #{param := of_the}, function) -> NamedFun(TheSecond, TheSecondToo, param)
    end.

different_clause("it doesn't happen on the first clause") ->
    not_here;
different_clause(But = "it does in the second one") ->
    fun ("it doesn't happen on the first clause") ->
            not_here;
        (ButToo = "it does in the second one") ->
            But == ButToo
    end.
