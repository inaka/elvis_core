-module(pass_state_record_and_type).

-dialyzer(no_behaviours).

-behaviour(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2
        ]).

-record(state, {}).

-type state() :: #state{}.

-spec init(term()) -> state().
init(_Args) ->
    #state{}.

-spec handle_call(term(), term(), state()) -> {noreply, state()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.
