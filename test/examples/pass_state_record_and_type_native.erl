-module(pass_state_record_and_type_native).

-if(?OTP_RELEASE >= 29).

-dialyzer(no_behaviours).

-behaviour(gen_server).

-export([
         init/1,
         handle_call/3,
         handle_cast/2
        ]).

-record #state{}.

-nominal state() :: #state{}.

-spec init(term()) -> state().
init(_Args) ->
    #state{}.

-spec handle_call(term(), term(), state()) -> {noreply, state()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-endif.
