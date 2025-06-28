-module(pass_used_ignored_variable_elvis_attr).

-export([ use_ignored_var/2
        , use_ignored_var_in_fun/2
        , no_used_ignored_vars_here/2, handle_call/3
        , macro_with_underscore/0
        ]).

-elvis([{elvis_style, no_used_ignored_variables, disable}]).
-elvis([{elvis_style, macro_names, #{regex => ".*"}}]).
-elvis([{elvis_style, no_macros, #{allow => ['__']}}]).

use_ignored_var(_One, Two) ->
    Three = _One + Two,
    case Three of
        3 ->
            three;
        _Four ->
            _Four
    end.

use_ignored_var_in_fun(_One, Two) ->
    Fun = fun (_Three) -> _One + _Three end,
    Fun(Two).

no_used_ignored_vars_here(One, _Two) ->
    {_Bla} = One.

-spec handle_call(Msg, _From, term()) ->
    {stop, {unknown_request, Msg}, {unknown_request, Msg}, term()}.
handle_call(Msg, _From, State) ->
    {stop, {unknown_request, Msg}, {unknown_request, Msg}, State}.

-define(__(X), X).

macro_with_underscore() ->
    ?__("This string will be translated").
