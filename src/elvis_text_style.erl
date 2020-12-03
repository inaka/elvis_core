-module(elvis_text_style).

-export([
         default/1,
         line_length/3,
         no_tabs/3,
         no_spaces/3,
         no_trailing_whitespace/3
        ]).

-define(LINE_LENGTH_MSG, "Line ~p is too long: ~s.").

-define(NO_TABS_MSG, "Line ~p has a tab at column ~p.").

-define(NO_SPACES_MSG, "Line ~p has a spaces at column ~p.").

-define(NO_TRAILING_WHITESPACE_MSG,
        "Line ~b has ~b trailing whitespace characters.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(Rule :: atom()) -> DefaultRuleConfig :: term().
default(line_length) ->
    #{ limit => 100
     , skip_comments => false
     };

default(no_tabs) ->
    #{};

default(no_trailing_whitespace) ->
    #{ ignore_empty_lines => false
     }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type line_length_config() :: #{ ignore => [elvis_config:ignorable()]
                               , limit => integer()
                               , skip_comments => false | any | whole_line
                               }.

%% @doc Target can be either a filename or the
%% name of a module.
-spec line_length(elvis_config:configs(),
                  elvis_file:file(),
                  line_length_config()) ->
    [elvis_result:item()].
line_length(_Config, Target, RuleConfig) ->
    Limit = option(limit, RuleConfig, line_length),
    SkipComments = option(skip_comments, RuleConfig, line_length),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Args = [Limit, SkipComments, Encoding],
    elvis_utils:check_lines(Src, fun check_line_length/3, Args).

-type no_tabs_config() :: #{ ignore => [module()]
                           }.

-spec no_tabs(elvis_config:configs(),
              elvis_file:file(),
              no_tabs_config()) ->
    [elvis_result:item()].
no_tabs(_Config, Target, _RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_no_tabs/3, []).

-type no_spaces_config() :: #{ ignore => [module()]
                             }.

-spec no_spaces(elvis_config:configs(),
                elvis_file:file(),
                no_spaces_config()) ->
    [elvis_result:item()].
no_spaces(_Config, Target, _RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    elvis_utils:check_lines(Src, fun check_no_spaces/3, []).

-type no_trailing_whitespace_config() :: #{ ignore => [module()],
                                            ignore_empty_lines => boolean()
                                          }.

-spec no_trailing_whitespace(Config::elvis_config:configs(),
                             Target::elvis_file:file(),
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Line Length

-spec line_is_comment(binary()) -> boolean().
line_is_comment(Line) ->
    case re:run(Line, "^[ \t]*%") of
        nomatch    -> false;
        {match, _} -> true
    end.

-spec remove_comment(binary()) -> binary().
remove_comment(Line) ->
    case re:run(Line, "([^%]+)", [{capture, first, binary}]) of
        nomatch            -> Line;
        {match, [Without]} -> Without
    end.

-spec check_line_length(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_line_length(Line, Num, [Limit, whole_line, Encoding]) ->
    case line_is_comment(Line) of
        false -> check_line_length(Line, Num, [Limit, Encoding]);
        true  -> no_result
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
            Info = [Num, binary_to_list(Line)],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result};
        _ ->
            no_result
    end.

%% No Tabs

-spec check_no_tabs(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_no_tabs(Line, Num, _Args) ->
    case binary:match(Line, <<"\t">>) of
        nomatch ->
            no_result;
        {Index, _} ->
            Msg = ?NO_TABS_MSG,
            Info = [Num, Index],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result}
    end.

%% No Spaces

-spec check_no_spaces(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_no_spaces(Line, Num, _Args) ->
    case re:run(Line, <<"^\t* ">>) of
        nomatch ->
            no_result;
        {Index, _} ->
            Msg = ?NO_SPACES_MSG,
            Info = [Num, Index],
            Result = elvis_result:new(item, Msg, Info, Num),
            {ok, Result}
    end.

%% No Trailing Whitespace

-spec check_no_trailing_whitespace(binary(), integer(), boolean()) ->
    no_result | {ok, elvis_result:item()}.
check_no_trailing_whitespace(Line, Num, IgnoreEmptyLines) ->
    Regex =
        case IgnoreEmptyLines of
            %% Lookbehind assertion: http://erlang.org/doc/man/re.html#sect17
            true -> "(?<=\\S)\\s+$";
            false -> "\\s+$"
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option(OptionName, RuleConfig, Rule) -> OptionValue
      when OptionName :: atom(),
           RuleConfig :: map(),
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
