-module(fail_state_record_and_type_gen_statem_state).

-dialyzer(no_behaviours).

-behaviour(gen_statem).

-export([
         init/1,
         handle_event/4,
         callback_mode/0
        ]).

-record(state1, {}).

-type state() :: #state1{}.

-spec init(term()) -> state().
init(_Args) ->
    #state1{}.

-spec handle_event(term(), term(), state(), term()) -> {next_state, state(), term()}.
handle_event(_EventType, _EventContent, State, Data) ->
    {next_state, State, Data}.

-spec callback_mode() -> handle_event_function.
callback_mode() ->
    handle_event_function.
