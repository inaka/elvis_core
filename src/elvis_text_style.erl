-module(elvis_text_style).

-export([default/1, line_length/3, no_tabs/3, no_trailing_whitespace/3,
         prefer_unquoted_atoms/3]).

-export_type([line_length_config/0, no_trailing_whitespace_config/0]).

-define(LINE_LENGTH_MSG, "Line ~p is too long. It has ~p characters.").
-define(NO_TABS_MSG, "Line ~p has a tab at column ~p.").
-define(NO_TRAILING_WHITESPACE_MSG, "Line ~b has ~b trailing whitespace characters.").
-define(ATOM_PREFERRED_QUOTES_MSG,
        "Atom ~p on line ~p is quoted "
        "but quotes are not needed.").

% These are part of a non-declared "behaviour"
% The reason why we don't try to handle them with different arity is
%  that arguments are ignored in different positions (1 and 3) so that'd
%  probably be messier than to ignore the warning
-hank([{unnecessary_function_arguments,
        [{no_trailing_whitespace, 3},
         {no_tabs, 3},
         {line_length, 3},
         {prefer_unquoted_atoms, 3}]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(Rule :: atom()) -> DefaultRuleConfig :: term().
default(line_length) ->
    #{limit => 100, skip_comments => false};
default(no_tabs) ->
    #{};
default(no_trailing_whitespace) ->
    #{ignore_empty_lines => false}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type line_length_config() ::
    #{ignore => [elvis_style:ignorable()],
      limit => integer(),
      skip_comments => false | any | whole_line}.

%% @doc Target can be either a filename or the
%% name of a module.
-spec line_length(elvis_config:config(), elvis_file:file(), line_length_config()) ->
                     [elvis_result:item()].
line_length(_Config, Target, RuleConfig) ->
    Limit = option(limit, RuleConfig, line_length),
    SkipComments = option(skip_comments, RuleConfig, line_length),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Args = [Limit, SkipComments, Encoding],
    elvis_utils:check_lines(Src, fun check_line_length/3, Args).

-spec no_tabs(elvis_config:config(),
              elvis_file:file(),
              elvis_style:empty_rule_config()) ->
                 [elvis_result:item()].
no_tabs(_Config, Target, _RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_no_tabs/2, []).

-type no_trailing_whitespace_config() ::
    #{ignore => [module()], ignore_empty_lines => boolean()}.

-spec no_trailing_whitespace(Config :: elvis_config:config(),
                             Target :: elvis_file:file(),
                             no_trailing_whitespace_config()) ->
                                [elvis_result:item()].
no_trailing_whitespace(_Config, Target, RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    IgnoreEmptyLines = option(ignore_empty_lines, RuleConfig, no_trailing_whitespace),
    elvis_utils:check_lines(Src,
                            fun(Src1, Fun, _Args) ->
                               check_no_trailing_whitespace(Src1, Fun, IgnoreEmptyLines)
                            end,
                            RuleConfig).

-spec prefer_unquoted_atoms(elvis_config:config(),
                            elvis_file:file(),
                            elvis_style:empty_rule_config()) ->
                               [elvis_result:item()].
prefer_unquoted_atoms(_Config, Target, _RuleConfig) ->
    {Content, #{encoding := _Encoding}} = elvis_file:src(Target),
    Tree = ktn_code:parse_tree(Content),
    AtomNodes = elvis_code:find(fun is_atom_node/1, Tree, #{traverse => all, mode => node}),
    check_atom_quotes(AtomNodes, []).

%% @private
check_atom_quotes([] = _AtomNodes, Acc) ->
    Acc;
check_atom_quotes([AtomNode | RemainingAtomNodes], AccIn) ->
    AtomName = ktn_code:attr(text, AtomNode),
    ValueAtomName = ktn_code:attr(value, AtomNode),

    IsException = is_exception_prefer_quoted(ValueAtomName),

    AccOut =
        case unicode:characters_to_list(AtomName, unicode) of
            [$' | _] when not IsException ->
                Msg = ?ATOM_PREFERRED_QUOTES_MSG,
                {Line, _} = ktn_code:attr(location, AtomNode),
                Info = [AtomName, Line],
                Result = elvis_result:new(item, Msg, Info, Line),
                AccIn ++ [Result];
            _ ->
                AccIn
        end,
    check_atom_quotes(RemainingAtomNodes, AccOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Line Length

%% @private
-spec line_is_comment(binary()) -> boolean().
line_is_comment(Line) ->
    case re:run(Line, "^[ \t]*%") of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

%% @private
-spec remove_comment(binary()) -> binary().
remove_comment(Line) ->
    case re:run(Line, "([^%]+)", [{capture, first, binary}]) of
        nomatch ->
            Line;
        {match, [Without]} ->
            Without
    end.

%% @private
-spec check_line_length(binary(), integer(), [term()]) ->
                           no_result | {ok, elvis_result:item()}.
check_line_length(Line, Num, [Limit, whole_line, Encoding]) ->
    case line_is_comment(Line) of
        false ->
            check_line_length(Line, Num, [Limit, Encoding]);
        true ->
            no_result
    end;
check_line_length(Line, Num, [Limit, any, Encoding]) ->
    LineWithoutComment = remove_comment(Line),
    check_line_length(LineWithoutComment, Num, [Limit, Encoding]);
check_line_length(Line, Num, [Limit, _, Encoding]) ->
    check_line_length(Line, Num, [Limit, Encoding]);
check_line_length(Line, Num, [Limit, Encoding]) ->
    Chars = unicode:characters_to_list(Line, Encoding),
    case length(Chars) of
        Large when Large > Limit ->
            Msg = ?LINE_LENGTH_MSG,
            Info = [Num, Large],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result};
        _ ->
            no_result
    end.

%% No Tabs

%% @private
-spec check_no_tabs(binary(), integer()) -> no_result | {ok, elvis_result:item()}.
check_no_tabs(Line, Num) ->
    case binary:match(Line, <<"\t">>) of
        nomatch ->
            no_result;
        {Index, _} ->
            Msg = ?NO_TABS_MSG,
            Info = [Num, Index],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result}
    end.

%% No Trailing Whitespace

%% @private
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
            Msg = ?NO_TRAILING_WHITESPACE_MSG,
            Info = [Num, size(binary:part(Line, PosLen))],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result}
    end.

%% @private
is_atom_node(MaybeAtom) ->
    ktn_code:type(MaybeAtom) =:= atom.

%% @private
is_exception_prefer_quoted(Elem) ->
    KeyWords =
        ['after', 'and', 'andalso', 'band', 'begin', 'bnot', 'bor', 'bsl', 'bsr', 'bxor', 'case',
         'catch', 'cond', 'div', 'end', 'fun', 'if', 'let', 'not', 'of', 'or', 'orelse', 'receive',
         'rem', 'try', 'when', 'xor'],
    lists:member(Elem, KeyWords).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option(OptionName, RuleConfig, Rule) -> OptionValue
    when OptionName :: atom(),
         RuleConfig :: elvis_core:rule_config(),
         Rule :: atom(),
         OptionValue :: term().
option(OptionName, RuleConfig, Rule) ->
    maybe_default_option(maps:get(OptionName, RuleConfig, undefined), OptionName, Rule).

-spec maybe_default_option(UserDefinedOptionValue, OptionName, Rule) -> OptionValue
    when UserDefinedOptionValue :: undefined | term(),
         OptionName :: atom(),
         Rule :: atom(),
         OptionValue :: term().
maybe_default_option(undefined = _UserDefinedOptionValue, OptionName, Rule) ->
    maps:get(OptionName, default(Rule));
maybe_default_option(UserDefinedOptionValue, _OptionName, _Rule) ->
    UserDefinedOptionValue.
