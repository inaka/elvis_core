-module(elvis_utils).

-compile({no_auto_import, [error/2]}).

%% General
-export([erlang_halt/1, list_to_str/1, to_str/1, split_all_lines/1, split_all_lines/2]).
%% Output / rebar3
-export([debug/2, info/2, notice/2, warn/2, error/2, abort/2]).

% These call (but verify if exported) rebar3-specific functions.
-ignore_xref(do_output/2).
-ignore_xref(abort/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec erlang_halt(integer()) -> no_return().
erlang_halt(Code) ->
    halt(Code).

-spec to_str(binary() | list() | atom() | integer()) -> string().
to_str(Arg) when is_binary(Arg) ->
    unicode:characters_to_list(Arg);
to_str(Arg) when is_atom(Arg) ->
    atom_to_list(Arg);
to_str(Arg) when is_integer(Arg) ->
    integer_to_list(Arg);
to_str(Arg) when is_list(Arg) ->
    Arg.

list_to_str(What) ->
    list_to_str(What, []).

list_to_str([], Acc) ->
    "[" ++ string:join(Acc, ", ") ++ "]";
list_to_str([H0 | T], Acc) ->
    H =
        case is_list(H0) of
            true ->
                "\"" ++ H0 ++ "\"";
            _ ->
                H0
        end,
    list_to_str(T, [to_str(H) | Acc]).

-spec split_all_lines(binary()) -> [binary(), ...].
split_all_lines(Binary) ->
    split_all_lines(Binary, []).

-spec split_all_lines(binary(), list()) -> [binary()].
split_all_lines(Binary, Opts) ->
    binary:split(Binary, [<<"\r\n">>, <<"\n">>], [global | Opts]).

-spec parse_colors(string()) -> string().
parse_colors(Message) ->
    Colors =
        #{
            "red" => "\e[0;31m",
            "red-bold" => "\e[1;31m",
            "green" => "\e[0;32m",
            "green-bold" => "\e[1;32m",
            "white" => "\e[0;37m",
            "white-bold" => "\e[1;37m",
            "magenta" => "\e[1;35m",
            "reset" => "\e[0m"
        },
    Opts = [global, {return, list}],
    case elvis_config:output_format() of
        P when P =:= plain; P =:= parsable ->
            re:replace(Message, "{{.*?}}", "", Opts);
        colors ->
            Fun = fun(Key, Acc) ->
                Regex = ["{{", Key, "}}"],
                Color = maps:get(Key, Colors),
                re:replace(Acc, Regex, Color, Opts)
            end,
            lists:foldl(Fun, Message, maps:keys(Colors))
    end.

-spec escape_chars(string()) -> string().
escape_chars(String) ->
    Binary = list_to_binary(String),
    Result = re:replace(Binary, "[^~]~", "~~", [global]),
    ResultBin = iolist_to_binary(Result),
    binary_to_list(ResultBin).

debug(Format, Data) ->
    output(debug, "Elvis: " ++ Format, Data).

info(Format, Data) ->
    output(info, Format ++ "{{reset}}~n", Data).

notice(Format, Data) ->
    output(notice, "{{white-bold}}" ++ Format ++ "{{reset}}~n", Data).

warn(Format, Data) ->
    output(warn, "{{magenta}}Warning: {{reset}}" ++ Format ++ "{{reset}}~n", Data).

error(Format, Data) ->
    output(error, "{{red}}Error: {{reset}}" ++ Format ++ "{{reset}}~n", Data).

-spec output(debug | info | notice | warn | error, Format :: io:format(), Data :: [term()]) -> ok.
output(debug = _Type, Format, Data) ->
    Chars = io_lib:format(Format, Data),
    do_output(debug, Chars);
output(Type, Format, Data) ->
    Chars = io_lib:format(Format, Data),
    EscapedChars = escape_chars(Chars),
    ColorParsedChars = parse_colors(EscapedChars),
    case elvis_config:no_output() of
        true ->
            ok;
        _ ->
            do_output(Type, ColorParsedChars)
    end.

-dialyzer({nowarn_function, do_output/2}).
do_output(debug, Chars) ->
    case erlang:function_exported(rebar_api, debug, 2) of
        true ->
            rebar_api:debug(Chars, []);
        false ->
            ok
    end;
do_output(info, Chars) ->
    case elvis_config:verbose() of
        true ->
            io:format(Chars);
        false ->
            ok
    end;
do_output(notice, Chars) ->
    io:format(Chars);
do_output(warn, Chars) ->
    io:format(Chars);
do_output(error, Chars) ->
    io:format(Chars).

-dialyzer({nowarn_function, abort/2}).
-spec abort(Format :: io:format(), Data :: [term()]) -> no_return().
abort(Format, Data) ->
    case erlang:function_exported(rebar_api, abort, 2) of
        true ->
            rebar_api:abort("Elvis: " ++ Format, [Data]);
        false ->
            output(error, Format, Data),
            throw(elvis_abort)
    end.
