-module(elvis_gitignore).

-export([required_patterns/3, forbidden_patterns/3]).

-define(REQUIRED_PATTERN, "Your .gitignore file should contain pattern '~s'.").
-define(FORBIDDEN_PATTERN, "Your .gitignore file should not contain pattern '~s'.").

-hank([{unnecessary_function_arguments, [{required_patterns, 3}, {forbidden_patterns, 3}]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(Rule :: atom()) -> DefaultRuleConfig :: term().
default(required_patterns) ->
    #{
        regexes =>
            [
                "^.rebar3/$",
                "^_build/$",
                "^_checkouts/$",
                "^doc/$",
                "^/erl_crash.dump$",
                "^/rebar3.crashdump$",
                "^test/logs/$"
            ]
    };
default(forbidden_patterns) ->
    #{regexes => ["^rebar.lock$"]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec required_patterns(
    elvis_config:config(),
    elvis_file:file(),
    elvis_style:empty_rule_config()
) ->
    [elvis_result:item()].
required_patterns(_Config, #{path := Path}, RuleConfig) ->
    Regexes = option(regexes, RuleConfig, required_patterns),
    case file:read_file(Path) of
        {ok, PatternsBin} ->
            Patterns = elvis_utils:split_all_lines(PatternsBin),
            check_patterns_in_lines(Patterns, Regexes, [], required);
        {error, _} ->
            []
    end.

-spec forbidden_patterns(
    elvis_config:config(),
    elvis_file:file(),
    elvis_style:empty_rule_config()
) ->
    [elvis_result:item()].
forbidden_patterns(_Config, #{path := Path}, RuleConfig) ->
    Regexes = option(regexes, RuleConfig, forbidden_patterns),
    case file:read_file(Path) of
        {ok, PatternsBin} ->
            Patterns = elvis_utils:split_all_lines(PatternsBin),
            check_patterns_in_lines(Patterns, Regexes, [], forbidden);
        {error, _} ->
            []
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% .gitignore
check_patterns_in_lines(_Lines, [], Results, _Mode) ->
    {ok, Results};
check_patterns_in_lines(Lines, [Pattern | Rest], Results0, Mode) ->
    ModeRespected =
        case Mode of
            required ->
                lists:any(fun(Line) -> re:run(Line, Pattern) =/= nomatch end, Lines);
            forbidden ->
                lists:all(fun(Line) -> re:run(Line, Pattern) =:= nomatch end, Lines)
        end,
    Results =
        case ModeRespected of
            true ->
                Results0;
            false when Mode =:= required ->
                [elvis_result:new(item, ?REQUIRED_PATTERN, [Pattern]) | Results0];
            false when Mode =:= forbidden ->
                [elvis_result:new(item, ?FORBIDDEN_PATTERN, [Pattern]) | Results0]
        end,
    check_patterns_in_lines(Lines, Rest, Results, Mode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option(OptionName, RuleConfig, Rule) -> OptionValue when
    OptionName :: atom(),
    RuleConfig :: elvis_config:config(),
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
