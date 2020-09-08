-module(pass_state_record_and_type_gen_statem).

-dialyzer(no_behaviours).

-behavior(gen_statem).

-export([
         init/1,
         handle_event/4
        ]).

-record(state, {}).

-type state1() :: #state{}.

-spec init(term()) -> state().
init(_Args) ->
    #state{}.

-spec handle_event(term(), term(), state1(), term()) -> {next_state, state1(), term()}.
handle_call(_EventType, _EventContent, State, Data) ->
    {next_state, State, Data}.
