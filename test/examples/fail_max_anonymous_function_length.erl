-module(fail_max_anonymous_function_length).

-export([fs/0]).

fs() ->
    #{
        f5 => fun(_) ->
                %% 1
                %% 2


                ok %% 5
              end,
        f10 => fun(_) -> %% 1
                    %% 2
                    %% 3
                    %% 4

                    %% 6
                    %% 7

                    %% 9
                    ok
               end,
        f15 => fun F15(_) -> %% 1
                    %% 2
                    %% 3
                    %% 4

                    %% 6
                    %% 7
                    %% 8
                    %% 9

                    %% 11
                    %% 12

                    %% 14
                    F15(ok) %% 15
               end
    }.
