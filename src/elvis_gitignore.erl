-module(elvis_gitignore).
-behaviour(elvis_ruleset).

-export([required_patterns/1, forbidden_patterns/1, default/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(RuleName :: atom()) -> elvis_core:rule_config().
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

required_patterns({_Config, #{path := Path}, _RuleConfig} = RuleCfg) ->
    Regexes = option(regexes, RuleCfg, required_patterns),
    case file:read_file(Path) of
        {ok, PatternsBin} ->
            Patterns = elvis_utils:split_all_lines(PatternsBin),
            check_patterns_in_lines(Patterns, Regexes, [], required);
        {error, _} ->
            []
    end.

forbidden_patterns({_Config, #{path := Path}, _RuleConfig} = RuleCfg) ->
    Regexes = option(regexes, RuleCfg, forbidden_patterns),
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
    Results;
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
                [
                    elvis_result:new_item(
                        "Your .gitignore file should contain pattern '~s'",
                        [Pattern]
                    )
                    | Results0
                ];
            false when Mode =:= forbidden ->
                [
                    elvis_result:new_item(
                        "Your .gitignore file should not contain pattern '~s'",
                        [Pattern]
                    )
                    | Results0
                ]
        end,
    check_patterns_in_lines(Lines, Rest, Results, Mode).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option(OptionName, RuleCfg, Rule) -> OptionValue when
    OptionName :: atom(),
    RuleCfg :: {Config, Target, RuleConfig},
    Config :: elvis_config:config(),
    Target :: elvis_file:file(),
    RuleConfig :: (Options :: #{atom() => term()}),
    Rule :: atom(),
    OptionValue :: term().
option(OptionName, {_Config, _Target, RuleConfig}, Rule) ->
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
