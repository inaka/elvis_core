-module(pass_state_record_and_type_plus_export_used_types_gen_statem).

-dialyzer(no_behaviours).

-behaviour(gen_statem).

-export([
         init/1,
         handle_event/4,
         callback_mode/0
        ]).

-export_type([state/0]).

-record(state, {}).

-opaque state() :: #state{}.

-spec init(term()) -> state().
init(_Args) ->
    #state{}.

-spec handle_event(term(), term(), state(), term()) -> {next_state, state(), term()}.
handle_event(_EventType, _EventContent, State, Data) ->
    {next_state, State, Data}.

-spec callback_mode() -> handle_event_function.
callback_mode() ->
    handle_event_function.
