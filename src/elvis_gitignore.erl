-module(elvis_gitignore).

-behaviour(elvis_ruleset).
-export([default/1]).

-export([required_patterns/1, forbidden_patterns/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(Rule :: atom()) -> elvis_core:rule_config().
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

required_patterns({_RuleNamespace, _Config, #{path := Path}, _RuleConfig} = RuleCfg) ->
    Regexes = elvis_ruleset:option(regexes, RuleCfg, ?FUNCTION_NAME),
    case file:read_file(Path) of
        {ok, PatternsBin} ->
            Patterns = elvis_utils:split_all_lines(PatternsBin),
            check_patterns_in_lines(Patterns, Regexes, [], required);
        {error, _} ->
            []
    end.

forbidden_patterns({_RuleNamespace, _Config, #{path := Path}, _RuleConfig} = RuleCfg) ->
    Regexes = elvis_ruleset:option(regexes, RuleCfg, ?FUNCTION_NAME),
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
