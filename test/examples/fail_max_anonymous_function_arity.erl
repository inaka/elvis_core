-module(fail_max_anonymous_function_arity).

-export([f/0]).

f() ->
    fun() ->
       fun(_) ->
          fun(_, _) ->
             fun(_, _, _) ->
                three_arguments
             end
          end
       end
    end.
