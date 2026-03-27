-module(pass_no_init_lists6).

%% Katana parses -if/-else/-endif without preprocessing, so the AST can
%% contain multiple `init/1` function forms (one per branch). The rule must
%% handle that without crashing.

-if(true).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

init({a, b}) ->
    {ok, #{}}.

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast(_, S) ->
    {noreply, S}.

-else.

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

init(_) ->
    {ok, #{}}.

handle_call(_, _, S) ->
    {reply, ok, S}.

handle_cast(_, S) ->
    {noreply, S}.

-endif.
