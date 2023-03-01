-module(pass_max_anonymous_function_arity).

-export([f/0]).

f() ->
    fun() ->
       fun(_) ->
          fun Two(1, 2) ->
                  Two(2, 3);
              Two(_, _) ->
                  fun(_, _, _) -> three_arguments end
          end
       end
    end.
