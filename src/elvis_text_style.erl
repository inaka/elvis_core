-module(elvis_text_style).

-behaviour(elvis_ruleset).
-export([default/1]).

-export([
    line_length/1,
    no_tabs/1,
    no_trailing_whitespace/1,
    prefer_unquoted_atoms/1,
    no_redundant_blank_lines/1
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(Rule :: atom()) -> elvis_core:rule_config().
default(line_length) ->
    #{
        limit => 100,
        skip_comments => false,
        no_whitespace_after_limit => true
    };
default(no_tabs) ->
    #{};
default(no_trailing_whitespace) ->
    #{ignore_empty_lines => false};
default(no_redundant_blank_lines) ->
    #{max_lines => 1};
default(prefer_unquoted_atoms) ->
    #{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Target can be either a filename or the
%% name of a module.
line_length({_Ruleset, _Config, Target, _RuleConfig} = RuleCfg) ->
    Limit = elvis_ruleset:option(limit, RuleCfg, ?FUNCTION_NAME),
    SkipComments = elvis_ruleset:option(skip_comments, RuleCfg, ?FUNCTION_NAME),
    NoWhitespace = elvis_ruleset:option(no_whitespace_after_limit, RuleCfg, ?FUNCTION_NAME),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Args = [Limit, SkipComments, Encoding, NoWhitespace],
    check_lines(Src, fun check_line_length/3, Args).

no_tabs({_Ruleset, _Config, Target, _RuleConfig}) ->
    {Src, _} = elvis_file:src(Target),
    check_lines(Src, fun check_no_tabs/2, []).

no_trailing_whitespace({_Ruleset, _Config, Target, RuleConfig} = RuleCfg) ->
    {Src, _} = elvis_file:src(Target),
    IgnoreEmptyLines = elvis_ruleset:option(ignore_empty_lines, RuleCfg, ?FUNCTION_NAME),
    check_lines(
        Src,
        fun(Src1, Fun, _Args) ->
            check_no_trailing_whitespace(Src1, Fun, IgnoreEmptyLines)
        end,
        RuleConfig
    ).

prefer_unquoted_atoms(RuleCfg) ->
    {nodes, AtomNodes} = elvis_code:find(#{
        of_types => [atom],
        inside => elvis_code:root(RuleCfg),
        filtered_by => fun doesnt_need_quotes/1,
        traverse => all
    }),

    lists:map(
        fun(AtomNode) ->
            elvis_result:new_item(
                "unnecessarily quoted atom ~s was found; prefer removing the quotes when "
                "not syntactically required",
                [ktn_code:attr(text, AtomNode)],
                #{node => AtomNode}
            )
        end,
        AtomNodes
    ).

doesnt_need_quotes(AtomNode) ->
    AtomName0 = ktn_code:attr(text, AtomNode),
    case re:run(AtomName0, "^'[a-z][a-zA-Z0-9_@]*'$", [{capture, none}]) of
        match ->
            AtomName = string:trim(AtomName0, both, "'"),
            Atom = list_to_atom(AtomName),
            Atom =/= 'maybe' andalso not erl_scan:f_reserved_word(Atom);
        _ ->
            false
    end.

no_redundant_blank_lines({_Ruleset, _Config, Target, _RuleConfig} = RuleCfg) ->
    MaxLines = elvis_ruleset:option(max_lines, RuleCfg, ?FUNCTION_NAME) + 1,
    {Src, _} = elvis_file:src(Target),
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
    BlankLines = lists:takewhile(fun(X) -> X == <<>> end, Lines),
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
