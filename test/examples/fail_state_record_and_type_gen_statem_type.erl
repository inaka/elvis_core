-module(fail_state_record_and_type_gen_statem_type).

-dialyzer(no_behaviours).

-behavior(gen_statem).

-export([
         init/1,
         handle_event/4,
         callback_mode/0
        ]).

-record(state, {}).

-type state1() :: #state{}.

-spec init(term()) -> state1().
init(_Args) ->
    #state{}.

-spec handle_event(term(), term(), state1(), term()) -> {next_state, state1(), term()}.
handle_event(_EventType, _EventContent, State, Data) ->
    {next_state, State, Data}.

-spec callback_mode() -> handle_event_function.
callback_mode() ->
    handle_event_function.
