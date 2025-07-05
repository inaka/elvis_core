-module(elvis_utils).

-compile({no_auto_import, [error/2]}).

%% General
-export([erlang_halt/1, to_str/1, split_all_lines/1, split_all_lines/2]).
%% Output
-export([info/2, notice/2, error/2, error_prn/2, warn_prn/2]).

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

-spec info(string(), [term()]) -> ok.
info(Message, Args) ->
    ColoredMessage = Message ++ "{{reset}}~n",
    print_info(ColoredMessage, Args).

-spec notice(string(), [term()]) -> ok.
notice(Message, Args) ->
    ColoredMessage = "{{white-bold}}" ++ Message ++ "{{reset}}~n",
    print_info(ColoredMessage, Args).

-spec error(string(), [term()]) -> ok.
error(Message, Args) ->
    ColoredMessage = "{{white-bold}}" ++ Message ++ "{{reset}}~n",
    print(ColoredMessage, Args).

-spec error_prn(string(), [term()]) -> ok.
error_prn(Message, Args) ->
    ColoredMessage = "{{red}}Error: {{reset}}" ++ Message ++ "{{reset}}~n",
    print(ColoredMessage, Args).

-spec warn_prn(string(), [term()]) -> ok.
warn_prn(Message, Args) ->
    ColoredMessage = "{{magenta}}Warning: {{reset}}" ++ Message ++ "{{reset}}~n",
    print(ColoredMessage, Args).

-spec print_info(string(), [term()]) -> ok.
print_info(Message, Args) ->
    case elvis_config:verbose() of
        true ->
            print(Message, Args);
        false ->
            ok
    end.

-spec print(string(), [term()]) -> ok.
print(Message, Args) ->
    case elvis_config:no_output() of
        true ->
            ok;
        _ ->
            Output = io_lib:format(Message, Args),
            EscapedOutput = escape_format_str(Output),
            io:format(parse_colors(EscapedOutput))
    end.

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

-spec escape_format_str(string()) -> string().
escape_format_str(String) ->
    Binary = list_to_binary(String),
    Result = re:replace(Binary, "[^~]~", "~~", [global]),
    ResultBin = iolist_to_binary(Result),
    binary_to_list(ResultBin).
