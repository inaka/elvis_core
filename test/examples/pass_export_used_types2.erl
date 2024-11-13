-module(pass_export_used_types2).

-behaviour(gen_server).

-record(state, {a = field}).

-type state() :: #state{}.

-export([init/1, handle_cast/2, handle_call/3]).

-spec init(_) -> {ok, state()}.
init(_) -> {ok, #state{}}.

handle_call(_, _, State) -> {State, ok, State}.

-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_, State) -> {noreply, State}.
