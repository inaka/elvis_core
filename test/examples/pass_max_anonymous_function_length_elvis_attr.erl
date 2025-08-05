-module(pass_max_anonymous_function_length_elvis_attr).

-export([fs/0]).

-elvis([{elvis_style, max_anonymous_function_length,
            #{ max_length => 15,
               count_comments => true,
               count_whitespace => true }}]).
fs() ->
    #{
        f5 => fun(_) -> %% 1
                %% 2
                %% 3

                ok
            end, %% 5
        f10 => fun(_) -> %% 1
            %% 2
            %% 3
            %% 4

            %% 6
            %% 7

            %% 9
            ok end, %% 10
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
