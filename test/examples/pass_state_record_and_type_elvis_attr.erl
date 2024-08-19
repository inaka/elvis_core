-module(pass_state_record_and_type_elvis_attr).

-dialyzer(no_behaviours).

-behaviour(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2
        ]).

-elvis([{elvis_style, state_record_and_type, disable}]).

-spec init(term()) -> ok.
init(_Args) ->
    ok.

-spec handle_call(term(), term(), term()) -> {noreply, term()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(term(), term()) -> {noreply, term()}.
handle_cast(_Request, State) ->
    {noreply, State}.
