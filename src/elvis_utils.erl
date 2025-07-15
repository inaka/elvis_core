-module(elvis_utils).

-compile({no_auto_import, [error/2]}).

%% General
-export([erlang_halt/1, to_str/1, split_all_lines/1, split_all_lines/2]).
%% Output / rebar3
-export([output/3, abort/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc This is defined so that it can be mocked for tests.
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
            rebar_api:debug("Elvis: " ++ Chars, []);
        false ->
            ok
    end;
do_output(info, Chars) ->
    case elvis_config:verbose() of
        true ->
            io:format(Chars ++ "{{reset}}~n");
        false ->
            ok
    end;
do_output(notice, Chars) ->
    case elvis_config:verbose() of
        true ->
            io:format("{{white-bold}}" ++ Chars ++ "{{reset}}~n");
        false ->
            ok
    end;
do_output(warn, Chars) ->
    io:format("{{magenta}}Warning: {{reset}}" ++ Chars ++ "{{reset}}~n");
do_output(error, Chars) ->
    io:format("{{red}}Error: {{reset}}" ++ Chars ++ "{{reset}}~n").

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
