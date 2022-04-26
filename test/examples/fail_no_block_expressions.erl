-module(fail_no_block_expressions).

-export([no_block_expression/0, block_expression/0]).

no_block_expression() ->
    ok.

block_expression() ->
    begin
        ok
    end.
