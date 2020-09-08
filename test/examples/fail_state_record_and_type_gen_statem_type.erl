-module(pass_state_record_and_type_gen_statem).

-dialyzer(no_behaviours).

-behavior(gen_statem).

-export([
         init/1,
         handle_event/4
        ]).

-record(state1, {}).

-type state1() :: #state1{}.

-spec init(term()) -> state().
init(_Args) ->
    #state1{}.

-spec handle_event(term(), term(), state1(), term()) -> {next_state, state1(), term()}.
handle_call(_EventType, _EventContent, State, Data) ->
    {next_state, State, Data}.
