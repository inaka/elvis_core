-module(fail_state_record_and_type_behaviour).

-dialyzer(no_behaviours).

-behaviour(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2
        ]).

-spec init(term()) -> ok.
init(_Args) ->
    ok.

-spec handle_call(term(), term(), term()) -> ok.
handle_call(_Request, _From, _State) ->
    ok.

-spec handle_cast(term(), term()) -> ok.
handle_cast(_Request, _State) ->
    ok.
