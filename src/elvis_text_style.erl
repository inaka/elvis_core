-module(elvis_text_style).
-behaviour(elvis_ruleset).

-export([
    default/1,
    line_length/3,
    no_tabs/3,
    no_trailing_whitespace/3,
    prefer_unquoted_atoms/3,
    no_redundant_blank_lines/3
]).

-export_type([line_length_config/0, no_trailing_whitespace_config/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(RuleName :: atom()) -> DefaultRuleConfig :: #{atom() := term()}.
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

-type line_length_config() ::
    #{
        ignore => [elvis_style:ignorable()],
        limit => integer(),
        skip_comments => false | any | whole_line
    }.

%% @doc Target can be either a filename or the
%% name of a module.
-spec line_length(elvis_config:config(), elvis_file:file(), line_length_config()) ->
    [elvis_result:item()].
line_length(_Config, Target, RuleConfig) ->
    Limit = option(limit, RuleConfig, line_length),
    SkipComments = option(skip_comments, RuleConfig, line_length),
    NoWhitespace = option(no_whitespace_after_limit, RuleConfig, line_length),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Args = [Limit, SkipComments, Encoding, NoWhitespace],
    elvis_utils:check_lines(Src, fun check_line_length/3, Args).

-spec no_tabs(
    elvis_config:config(),
    elvis_file:file(),
    elvis_style:empty_rule_config()
) ->
    [elvis_result:item()].
no_tabs(_Config, Target, _RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_no_tabs/2, []).

-type no_trailing_whitespace_config() ::
    #{ignore => [module()], ignore_empty_lines => boolean()}.

-spec no_trailing_whitespace(
    Config :: elvis_config:config(),
    Target :: elvis_file:file(),
    no_trailing_whitespace_config()
) ->
    [elvis_result:item()].
no_trailing_whitespace(_Config, Target, RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    IgnoreEmptyLines = option(ignore_empty_lines, RuleConfig, no_trailing_whitespace),
    elvis_utils:check_lines(
        Src,
        fun(Src1, Fun, _Args) ->
            check_no_trailing_whitespace(Src1, Fun, IgnoreEmptyLines)
        end,
        RuleConfig
    ).

-spec prefer_unquoted_atoms(
    elvis_config:config(),
    elvis_file:file(),
    elvis_style:empty_rule_config()
) ->
    [elvis_result:item()].
prefer_unquoted_atoms(_Config, Target, _RuleConfig) ->
    {Content, #{encoding := _Encoding}} = elvis_file:src(Target),
    Tree = ktn_code:parse_tree(Content),
    AtomNodes = elvis_code:find_by_types([atom], Tree, #{traverse => all, mode => node}),
    check_atom_quotes(AtomNodes, []).

needs_quoting(AtomName0) ->
    case re:run(AtomName0, "^'[a-z][a-zA-Z0-9_@]*'$", [{capture, none}]) of
        match ->
            AtomName = string:trim(AtomName0, both, "'"),
            Atom = list_to_atom(AtomName),
            Atom =:= 'maybe' orelse erl_scan:f_reserved_word(Atom);
        _ ->
            true
    end.

check_atom_quotes([] = _AtomNodes, Acc) ->
    Acc;
check_atom_quotes([AtomNode | RemainingAtomNodes], AccIn) ->
    AtomName = ktn_code:attr(text, AtomNode),

    AccOut =
        case needs_quoting(AtomName) of
            false ->
                AccIn ++
                    [
                        elvis_result:new_item(
                            "atom ~p is quoted but quotes are not needed",
                            [AtomName],
                            #{node => AtomNode}
                        )
                    ];
            _ ->
                AccIn
        end,
    check_atom_quotes(RemainingAtomNodes, AccOut).

no_redundant_blank_lines(_Config, Target, RuleConfig) ->
    MaxLines = option(max_lines, RuleConfig, ?FUNCTION_NAME) + 1,
    {Src, _} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    Result = redundant_blank_lines(Lines, {1, []}),

    ResultFun =
        fun
            ({Line, BlankLinesLength}) when BlankLinesLength >= MaxLines ->
                {true,
                    elvis_result:new_item(
                        "there are too many blank lines",
                        [],
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
            {ok, line_length_res(Num, Len)};
        Len ->
            case binary:match(Line, <<"\s">>, [{scope, {Limit, Len - Limit}}]) of
                {_, _} ->
                    {ok, line_length_res(Num, Len)};
                nomatch ->
                    no_result
            end
    end.

line_length_res(Num, Len) ->
    elvis_result:new_item(
        "there are too many characters",
        [],
        #{line => Num, limit => Len}
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
                    "there is a tab at column ~p",
                    [Index],
                    #{line => Num}
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
        {match, [PosLen]} ->
            {ok,
                elvis_result:new_item(
                    "there are too trailing whitespace characters",
                    [],
                    #{line => Num, limit => byte_size(binary:part(Line, PosLen))}
                )}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option(OptionName, RuleConfig, Rule) -> OptionValue when
    OptionName :: atom(),
    RuleConfig :: elvis_core:rule_config(),
    Rule :: atom(),
    OptionValue :: term().
option(OptionName, RuleConfig, Rule) ->
    maybe_default_option(maps:get(OptionName, RuleConfig, undefined), OptionName, Rule).

-spec maybe_default_option(UserDefinedOptionValue, OptionName, Rule) -> OptionValue when
    UserDefinedOptionValue :: undefined | term(),
    OptionName :: atom(),
    Rule :: atom(),
    OptionValue :: term().
maybe_default_option(undefined = _UserDefinedOptionValue, OptionName, Rule) ->
    maps:get(OptionName, default(Rule));
maybe_default_option(UserDefinedOptionValue, _OptionName, _Rule) ->
    UserDefinedOptionValue.
