-module(fail_max_anonymous_function_arity).

-export([f/0]).

f() ->
    fun() ->
       fun(_) ->
          fun NamedFun(_, _) ->
            NamedFun(
               fun(_, _, _) ->
                  three_arguments
               end,
               two_arguments
            )
          end
       end
    end.
