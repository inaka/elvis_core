-module(elvis_text_style).

-behaviour(elvis_rule).
-export([default/1]).

-export([
    line_length/2,
    no_tabs/2,
    no_trailing_whitespace/2,
    no_redundant_blank_lines/2
]).

% The whole file is considered to have either callback functions or rules.
-ignore_xref(elvis_text_style).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(RuleName :: atom()) -> elvis_rule:def().
default(line_length) ->
    elvis_rule:defmap(#{
        limit => 100,
        skip_comments => false,
        no_whitespace_after_limit => true
    });
default(no_trailing_whitespace) ->
    elvis_rule:defmap(#{
        ignore_empty_lines => false
    });
default(no_redundant_blank_lines) ->
    elvis_rule:defmap(#{
        max_lines => 1
    });
default(_RuleName) ->
    elvis_rule:defmap(#{}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc File can be either a filename or the
%% name of a module.
-spec line_length(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
line_length(Rule, _ElvisConfig) ->
    Limit = elvis_rule:option(limit, Rule),
    SkipComments = elvis_rule:option(skip_comments, Rule),
    NoWhitespace = elvis_rule:option(no_whitespace_after_limit, Rule),
    {Src, File} = elvis_file:src(elvis_rule:file(Rule)),
    Encoding = elvis_file:encoding(File),
    Args = [Limit, SkipComments, Encoding, NoWhitespace],
    check_lines(Src, fun check_line_length/3, Args).

-spec no_tabs(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_tabs(Rule, _ElvisConfig) ->
    {Src, _} = elvis_file:src(elvis_rule:file(Rule)),
    check_lines(Src, fun check_no_tabs/2, []).

-spec no_trailing_whitespace(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_trailing_whitespace(Rule, _ElvisConfig) ->
    {Src, _} = elvis_file:src(elvis_rule:file(Rule)),
    IgnoreEmptyLines = elvis_rule:option(ignore_empty_lines, Rule),
    check_lines(
        Src,
        fun(Src1, Fun, _Args) ->
            check_no_trailing_whitespace(Src1, Fun, IgnoreEmptyLines)
        end,
        elvis_rule:def(Rule)
    ).

-spec no_redundant_blank_lines(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_redundant_blank_lines(Rule, _ElvisConfig) ->
    MaxLines = elvis_rule:option(max_lines, Rule) + 1,
    {Src, _} = elvis_file:src(elvis_rule:file(Rule)),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    Result = redundant_blank_lines(Lines, {1, []}),

    ResultFun =
        fun
            ({Line, BlankLinesLength}) when BlankLinesLength >= MaxLines ->
                {true,
                    elvis_result:new_item(
                        "there are too many (~p) blank lines; prefer respecting the configured "
                        "limit",
                        [BlankLinesLength],
                        #{line => Line, limit => BlankLinesLength}
                    )};
            (_) ->
                false
        end,
    lists:filtermap(ResultFun, Result).

redundant_blank_lines([], {_, Result}) ->
    Result;
redundant_blank_lines(Lines, {CurrentLineNum, ResultList}) ->
    BlankLines = lists:takewhile(fun(X) -> X =:= <<>> end, Lines),
    BlankElements = length(BlankLines),
    Index =
        case BlankElements of
            0 ->
                1;
            Num ->
                Num
        end,
    NextLineNum = CurrentLineNum + Index,
    case BlankElements of
        0 ->
            redundant_blank_lines(lists:nthtail(Index, Lines), {NextLineNum, ResultList});
        _ ->
            redundant_blank_lines(
                lists:nthtail(Index, Lines),
                {NextLineNum, [{CurrentLineNum, BlankElements} | ResultList]}
            )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Line Length

-spec line_is_comment(binary()) -> boolean().
line_is_comment(Line) ->
    re:run(Line, "^[ \t]*%") =/= nomatch.

-spec remove_comment(binary()) -> binary().
remove_comment(Line) ->
    case re:run(Line, "([^%]+)", [{capture, first, binary}]) of
        nomatch ->
            Line;
        {match, [Without]} ->
            Without
    end.

-spec check_line_length(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_line_length(Line, Num, [Limit, whole_line, Encoding, NoWhitespace]) ->
    case line_is_comment(Line) of
        false ->
            check_line_length(Line, Num, [Limit, Encoding, NoWhitespace]);
        true ->
            no_result
    end;
check_line_length(Line, Num, [Limit, any, Encoding, NoWhitespace]) ->
    LineWithoutComment = remove_comment(Line),
    check_line_length(LineWithoutComment, Num, [Limit, Encoding, NoWhitespace]);
check_line_length(Line, Num, [Limit, _, Encoding, NoWhitespace]) ->
    check_line_length(Line, Num, [Limit, Encoding, NoWhitespace]);
check_line_length(Line0, Num, [Limit, Encoding, NoWhitespace]) ->
    Line = unicode:characters_to_binary(Line0, Encoding),
    case string:length(Line) of
        Len when Len =< Limit ->
            no_result;
        Len when NoWhitespace ->
            {ok, line_length_res(Num, Len, Limit)};
        Len ->
            case binary:match(Line, <<"\s">>, [{scope, {Limit, Len - Limit}}]) of
                {_, _} ->
                    {ok, line_length_res(Num, Len, Limit)};
                nomatch ->
                    no_result
            end
    end.

line_length_res(Num, Len, Limit) ->
    elvis_result:new_item(
        "there are too many (~p) characters; prefer respecting the configured limit",
        [Len],
        #{line => Num, limit => Limit}
    ).

%% No Tabs

-spec check_no_tabs(binary(), integer()) -> no_result | {ok, elvis_result:item()}.
check_no_tabs(Line, Num) ->
    case binary:match(Line, <<"\t">>) of
        nomatch ->
            no_result;
        {Index, _} ->
            {ok,
                elvis_result:new_item(
                    "an unexpected tab character was found; prefer spaces",
                    [],
                    #{line => Num, column => Index}
                )}
    end.

%% No Trailing Whitespace

-spec check_no_trailing_whitespace(binary(), integer(), boolean()) ->
    no_result | {ok, elvis_result:item()}.
check_no_trailing_whitespace(Line, Num, IgnoreEmptyLines) ->
    Regex =
        case IgnoreEmptyLines of
            %% Lookbehind assertion: https://erlang.org/doc/man/re.html#sect17
            true ->
                "(?<=\\S)\\s+$";
            false ->
                "\\s+$"
        end,

    case re:run(Line, Regex) of
        nomatch ->
            no_result;
        {match, _} ->
            {ok,
                elvis_result:new_item(
                    "unexpected trailing whitespace was found",
                    [],
                    #{line => Num}
                )}
    end.

%% @doc Takes a binary that holds source code and applies
%%      Fun to each line. Fun takes 2 or 3 arguments (the line
%%      as a binary, the line number and the optional supplied Args) and
%%      returns 'no_result' or {'ok', Result}.
-spec check_lines(binary(), fun(), term()) -> [elvis_result:item()].
check_lines(Src, Fun, Args) ->
    Lines = elvis_utils:split_all_lines(Src),
    check_lines(Lines, Fun, Args, [], 1).

check_lines([], _Fun, _Args, Results, _Num) ->
    lists:flatten(
        lists:reverse(Results)
    );
check_lines([Line | Lines], Fun, Args, Results, Num) ->
    FunRes =
        case is_function(Fun, 3) of
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
