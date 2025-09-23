-module(pass_prefer_include).

-include("prefer_include.hrl").
-include_lib("test/examples/prefer_include_lib.hrl").

-export([main/1]).

main(1) -> ?PREFER_INCLUDE;
main(2) -> ?PREFER_INCLUDE_LIB.
