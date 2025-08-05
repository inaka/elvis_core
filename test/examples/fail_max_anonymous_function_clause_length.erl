-module(fail_max_anonymous_function_clause_length).

-export([fs/0]).

fs() ->
    #{
        f5 => fun
            (a) ->
                %% 2
                %% 3

                %% 5
                ok;
            (b) ->
                %% 2
                %% 3
                %% 4

                %% 6
                %% 7

                %% 9



                %% 13
                ok;
            (c) ->
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
                ok
        end,
        f6 => fun
            F6(d) ->
                %% 1
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

                ok;
            F6(e) ->
                ok;
            F6(f) ->
                F6(ok)
        end,
        f7 => fun
            (a) -> 1;
            (b) -> 2;
            (c) -> 3;
            (d) -> 4;
            (e) -> 5;
            (f) -> 6;
            (aa) -> 7;
            (bb) ->
                8;
            (cc) -> 9;
            (dd) ->
                10;
            (ee) -> 11;
            (ff) -> 12;
            (aaa) -> 13;
            (bbb) -> 14;
            (ccc) -> 15;
            (ddd) -> 16;
            (eee) -> 17;
            (fff) -> 18;
            (aaaa) -> 19;
            (bbbb) ->
                20;
            (cccc) -> 21;
            (dddd) -> 22;
            (eeee) -> 23;
            (ffff) ->
                24;
            (aaaaa) -> 25
        end
    }.
