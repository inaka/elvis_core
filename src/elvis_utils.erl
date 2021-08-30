-module(elvis_utils).

-compile({no_auto_import, [error/2]}).

-export([
         %% Rules
         check_lines/3,
         check_lines_with_context/4,
         indentation/3,
         check_nodes/3,

         %% General
         erlang_halt/1,
         to_str/1,
         split_all_lines/1,
         split_all_lines/2,

         %% Output
         info/1,
         info/2,
         notice/1,
         notice/2,
         error/1,
         error/2,
         error_prn/1,
         error_prn/2,
         warn_prn/2,
         parse_colors/1
        ]).

-export_type([file/0]).

-type file() :: #{path => string(), content => binary()}.
-type line_content() :: {integer(), integer()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Takes a binary that holds source code and applies
%%      Fun to each line. Fun takes 2 or 3 arguments (the line
%%      as a binary, the line number and the optional supplied Args) and
%%      returns 'no_result' or {'ok', Result}.
-spec check_lines(binary(), fun(), term()) ->
    [elvis_result:item()].
check_lines(Src, Fun, Args) ->
    Lines = split_all_lines(Src),
    check_lines(Lines, Fun, Args, [], 1).

%% @doc Checks each line calling fun and providing the previous and next
%%      lines based on the context tuple {Before, After}.
-spec check_lines_with_context(binary(), fun(), term(), line_content()) ->
    [elvis_result:item()].
check_lines_with_context(Src, Fun, Args, Ctx) ->
    Lines = split_all_lines(Src),
    LinesContext = context(Lines, Ctx),
    check_lines(LinesContext, Fun, Args, [], 1).

%% @private
check_lines([], _Fun, _Args, Results, _Num) ->
    lists:flatten(lists:reverse(Results));
check_lines([Line | Lines], Fun, Args, Results, Num) ->
    FunRes
        = case is_function(Fun, 3) of
              true ->
                  Fun(Line, Num, Args);
              false ->
                  Fun(Line, Num)
          end,
    case FunRes of
        {ok, Result} ->
            check_lines(Lines, Fun, Args, [Result | Results], Num + 1);
        no_result ->
            check_lines(Lines, Fun, Args, Results, Num + 1)
    end.

%% @private
context(List, CtxCount) ->
    context(List, [], CtxCount, []).

context([], _Past, _CtxCount, Results) ->
    lists:reverse(Results);
context([Current | Future], Past, CtxCount = {PrevCount, NextCount}, Results) ->
    Prev = lists:sublist(Past, PrevCount),
    Next = lists:sublist(Future, NextCount),
    Item = {Current, lists:reverse(Prev), Next},
    context(Future, [Current | Past], CtxCount, [Item | Results]).

%% @doc Takes a binary that holds source code and applies
%% Fun to each line. Fun takes 3 arguments (the line
%% as a binary, the line number and the supplied Args) and
%% returns 'no_result' or {'ok', Result}.
-spec check_nodes(ktn_code:tree_node(), fun(), [term()]) ->
    [elvis_result:item()].
check_nodes(RootNode, Fun, Args) ->
    ChildNodes = ktn_code:content(RootNode),
    check_nodes(ChildNodes, Fun, Args, []).

%% @private
check_nodes([], _Fun, _Args, Results) ->
    FlatResults = lists:flatten(Results),
    lists:reverse(FlatResults);
check_nodes([Node | Nodes], Fun, Args, Results) ->
    case Fun(Node, Args) of
        [] ->
            check_nodes(Nodes, Fun, Args, Results);
        Result ->
            check_nodes(Nodes, Fun, Args, [Result | Results])
    end.

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

-spec split_all_lines(binary()) -> [binary()].
split_all_lines(Binary) ->
    split_all_lines(Binary, []).

-spec split_all_lines(binary(), list()) -> [binary()].
split_all_lines(Binary, Opts) ->
    binary:split(Binary, [<<"\r\n">>, <<"\n">>], [global | Opts]).

%% @doc Takes a line, a character and a count, returning the indentation level
%%      invalid if the number of character is not a multiple of count.
-spec indentation(binary() | string(), char(), integer()) ->
    invalid | integer().
indentation(Line, Char, Count) ->
    LineStr = to_str(Line),
    Regex = "^" ++ [Char] ++ "*",
    {match, [{0, Len} | _]} = re:run(LineStr, Regex),
    case Len rem Count of
        0 -> Len div Count;
        _ -> invalid
    end.

-spec info(string()) -> ok.
info(Message) ->
    info(Message, []).

-spec info(string(), [term()]) -> ok.
info(Message, Args) ->
    ColoredMessage = Message ++ "{{reset}}~n",
    print_info(ColoredMessage, Args).

-spec notice(string()) -> ok.
notice(Message) ->
    notice(Message, []).

-spec notice(string(), [term()]) -> ok.
notice(Message, Args) ->
    ColoredMessage = "{{white-bold}}" ++ Message ++ "{{reset}}~n",
    print_info(ColoredMessage, Args).

-spec error(string()) -> ok.
error(Message) ->
    error(Message, []).

-spec error(string(), [term()]) -> ok.
error(Message, Args) ->
    ColoredMessage = "{{white-bold}}" ++ Message ++ "{{reset}}~n",
    print(ColoredMessage, Args).

-spec error_prn(string()) -> ok.
error_prn(Message) ->
    error_prn(Message, []).

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
    case elvis_config:from_application_or_config(verbose, false) of
        true -> print(Message, Args);
        false -> ok
    end.

-spec print(string(), [term()]) -> ok.
print(Message, Args) ->
    case elvis_config:from_application_or_config(no_output, false) of
        true -> ok;
        _ ->
            Output = io_lib:format(Message, Args),
            EscapedOutput = escape_format_str(Output),
            io:format(parse_colors(EscapedOutput))
    end.


-spec parse_colors(string()) -> string().
parse_colors(Message) ->
    Colors = #{"red" => "\e[0;31m",
               "red-bold" => "\e[1;31m",
               "green" => "\e[0;32m",
               "green-bold" => "\e[1;32m",
               "white" => "\e[0;37m",
               "white-bold" => "\e[1;37m",
               "magenta" => "\e[1;35m",
               "reset" => "\e[0m"},
    Opts = [global, {return, list}],
    case elvis_config:from_application_or_config(output_format, colors) of
        P when P =:= plain orelse
               P =:= parsable ->
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
