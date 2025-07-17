-module(elvis_config).

-feature(maybe_expr, enable).

-export([from_rebar/1, from_file/1, validate_config/1, default/0]).
%% Getters
-export([dirs/1, ignore/1, filter/1, files/1, rules/1]).
%% Files
-export([resolve_files/1, resolve_files/2, apply_to_files/2]).
%% Rules
-export([merge_rules/2]).

%% Options
-export([config/0, output_format/0, verbose/0, no_output/0, parallel/0]).
-export([set_output_format/1, set_verbose/1, set_no_output/1, set_parallel/1]).

% Corresponds to the 'config' key.
-type t() :: map().
-export_type([t/0]).

-type elvis() :: proplists:proplist().
-export_type([elvis/0]).

% API exports, not consumed locally.
-ignore_xref([from_rebar/1, from_file/1, default/0, resolve_files/2, apply_to_files/2]).
-ignore_xref([set_output_format/1, set_verbose/1, set_no_output/1, set_parallel/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fetch_elvis_config(Key, AppConfig) ->
    Elvis = from_static(elvis, {app, AppConfig}),
    _ =
        case lists:member(Key, elvis_control_opts()) of
            true ->
                ok;
            _ ->
                do_validate({elvis, Elvis})
        end,
    _ = elvis_ruleset:load_custom(from_static(rulesets, {elvis, Elvis})),
    Elvis.

from_static(Key, {Type, Config}) ->
    elvis_utils:debug("fetching key '~s' from '~s' configuration", [Key, Type]),
    case proplists:get_value(Key, Config) of
        undefined ->
            elvis_utils:debug(
                "no value for key '~s' found in '~s' configuration; going with default", [
                    Key, Type
                ]
            ),
            default(Key);
        Value ->
            elvis_utils:debug("value for key '~s' found in '~s' configuration", [Key, Type]),
            Value
    end.

config() ->
    for(config).

output_format() ->
    for(output_format).

verbose() ->
    for(verbose).

no_output() ->
    for(no_output).

parallel() ->
    for(parallel).

set_output_format(OutputFormat) ->
    set_env(output_format, OutputFormat).

set_verbose(Verbose) ->
    set_env(verbose, Verbose).

set_no_output(NoOutput) ->
    set_env(no_output, NoOutput).

set_parallel(Parallel) ->
    set_env(parallel, Parallel).

set_env(Key, Value) ->
    application:set_env(elvis_core, Key, Value).

default(Key) ->
    case application:get_env(elvis_core, Key) of
        undefined ->
            elvis_utils:debug(
                "no value for key '~s' found in application environment; going with default",
                [Key]
            ),
            default_for(Key);
        {ok, Value} ->
            elvis_utils:debug("value for key '~s' found in application environment", [Key]),
            Value
    end.

for(Key) ->
    AppDefault = default_for(app),
    AppConfig =
        case consult_elvis_config("elvis.config") of
            AppDefault ->
                % This might happen whether we fail to parse the file or it actually is []
                elvis_utils:debug("elvis.config unusable; falling back to rebar.config", []),
                consult_rebar_config("rebar.config");
            AppConfig0 ->
                AppConfig0
        end,
    % If we got this far, the configuration is valid...
    Elvis = fetch_elvis_config(Key, AppConfig),
    from_static(Key, {elvis, Elvis}).

consult_elvis_config(File) ->
    case file:consult(File) of
        {ok, [AppConfig]} ->
            elvis_utils:debug("elvis.config usable; using it", []),
            AppConfig;
        _ ->
            elvis_utils:debug("elvis.config unusable", []),
            default_for(app)
    end.

consult_rebar_config(File) ->
    case file:consult(File) of
        {ok, AppConfig0} ->
            elvis_utils:debug("rebar.config usable; using it", []),
            AppConfig0;
        _ ->
            elvis_utils:debug("rebar.config unusable", []),
            default_for(app)
    end.

-spec from_rebar(File :: string()) -> [t()].
from_rebar(File) ->
    AppConfig = consult_rebar_config(File),
    fetch_elvis_config_from(AppConfig).

-spec from_file(File :: string()) -> [t()].
from_file(File) ->
    AppConfig = consult_elvis_config(File),
    fetch_elvis_config_from(AppConfig).

fetch_elvis_config_from(AppConfig) ->
    try fetch_elvis_config(undefined, AppConfig) of
        Elvis ->
            from_static(config, {elvis, Elvis})
    catch
        {invalid_config, _} = Caught ->
            {fail, [{throw, Caught}]}
    end.

default_for(app) ->
    % This is the top-level element, before 'elvis'
    [];
default_for(elvis) ->
    [];
default_for(config) ->
    [];
default_for(output_format) ->
    colors;
default_for(verbose) ->
    false;
default_for(no_output) ->
    false;
default_for(parallel) ->
    1;
default_for(rulesets) ->
    #{};
default_for([config, dirs]) ->
    [];
default_for([config, filter]) ->
    "";
default_for([config, ignore]) ->
    [];
default_for([config, ruleset]) ->
    undefined;
default_for([config, rules]) ->
    [].

default() ->
    [
        #{
            dirs => [
                "apps/**/src/**",
                "src/**"
            ],
            filter => "*.erl",
            ruleset => erl_files
        },
        #{
            dirs => [
                "apps/**/src/**",
                "src/**",
                "apps/**/include/**",
                "include/**"
            ],
            filter => "*.hrl",
            ruleset => hrl_files
        },
        #{
            dirs => ["."],
            filter => "rebar.config",
            ruleset => rebar_config
        },
        #{
            dirs => ["."],
            filter => ".gitignore",
            ruleset => gitignore
        }
    ].

-spec dirs(Config :: [t()] | t()) -> [string()].
dirs(Config) when is_list(Config) ->
    lists:flatmap(fun dirs/1, Config);
dirs(#{dirs := Dirs}) ->
    Dirs;
dirs(#{}) ->
    [].

-spec ignore([t()] | t()) -> [string()].
ignore(Config) when is_list(Config) ->
    lists:flatmap(fun ignore/1, Config);
ignore(#{ignore := Ignore}) ->
    lists:map(fun ignore_to_regexp/1, Ignore);
ignore(#{}) ->
    [].

-spec filter([t()] | t()) -> [string()].
filter(Config) when is_list(Config) ->
    lists:flatmap(fun filter/1, Config);
filter(#{filter := Filter}) ->
    Filter;
filter(#{}) ->
    "*.erl".

-spec files(RuleGroup :: [t()] | t()) -> [elvis_file:t()].
files(RuleGroup) when is_list(RuleGroup) ->
    lists:map(fun files/1, RuleGroup);
files(#{files := Files}) ->
    Files;
files(#{}) ->
    [].

-spec rules
    (RulesL :: [t()]) -> [[elvis_rule:t()]];
    (Rules :: t()) -> [elvis_rule:t()].
rules(Rules) when is_list(Rules) ->
    lists:map(fun rules/1, Rules);
rules(#{rules := UserRules, ruleset := Ruleset}) ->
    DefaultRules = elvis_ruleset:rules(Ruleset),
    merge_rules(UserRules, DefaultRules);
rules(#{rules := Rules}) ->
    Rules;
rules(#{ruleset := Ruleset}) ->
    elvis_ruleset:rules(Ruleset);
rules(#{}) ->
    [].

%% @doc Takes a configuration and a list of files, filtering some
%%      of them according to the 'filter' key, or if not specified
%%      uses '*.erl'.
%% @end
%% resolve_files/2 with a [t()] type is used in elvis project
-spec resolve_files(Config :: [t()] | t(), Files :: [elvis_file:t()]) ->
    [t()] | t().
resolve_files(Config, Files) when is_list(Config) ->
    Fun = fun(RuleGroup) -> resolve_files(RuleGroup, Files) end,
    lists:map(Fun, Config);
resolve_files(RuleGroup, Files) ->
    Filter = filter(RuleGroup),
    Dirs = dirs(RuleGroup),
    Ignore = ignore(RuleGroup),
    FilteredFiles = elvis_file:filter_files(Files, Dirs, Filter, Ignore),
    RuleGroup#{files => FilteredFiles}.

%% @doc Takes a configuration and finds all files according to its 'dirs'
%%      end  'filter' key, or if not specified uses '*.erl'.
%% @end
-spec resolve_files(t()) -> t().
resolve_files(#{files := _Files} = RuleGroup) ->
    RuleGroup;
resolve_files(#{dirs := Dirs} = RuleGroup) ->
    Filter = filter(RuleGroup),
    Files = elvis_file:find_files(Dirs, Filter),
    resolve_files(RuleGroup, Files);
resolve_files(#{}) ->
    [].

%% @doc Takes a function and configuration and applies the function to all
%%      file in the configuration.
%% @end
-spec apply_to_files(Fun :: fun(), Config :: [t()] | t()) ->
    [t()] | t().
apply_to_files(Fun, Config) when is_list(Config) ->
    ApplyFun = fun(RuleGroup) -> apply_to_files(Fun, RuleGroup) end,
    lists:map(ApplyFun, Config);
apply_to_files(Fun, #{files := Files} = RuleGroup) ->
    NewFiles = lists:map(Fun, Files),
    RuleGroup#{files => NewFiles}.

%% @doc Ensures the ignore is a regexp, this is used
%%      to allow using 'module name' atoms in the ignore
%%      list by taking advantage of the fact that erlang
%%      enforces the module and the file name to be the
%%      same.
%% @end
-spec ignore_to_regexp(string() | atom()) -> string().
ignore_to_regexp(R) when is_list(R) ->
    R;
ignore_to_regexp(A) when is_atom(A) ->
    "/" ++ atom_to_list(A) ++ "\\.erl$".

%% @doc Merge user rules (override) with elvis default rules.
-spec merge_rules(UserRules :: list(), DefaultRules :: list()) -> [elvis_rule:t()].
merge_rules(UserRules, DefaultRules) ->
    UnduplicatedRules =
        % Drops repeated rules

        % If any default rule is in UserRules it means the user
        lists:filtermap(
            % wants to override the rule.
            fun(Tuple) ->
                Rule = elvis_rule:from_tuple(Tuple),
                case not is_rule_override(Rule, UserRules) andalso not elvis_rule:disabled(Rule) of
                    true ->
                        {true, Rule};
                    false ->
                        false
                end
            end,
            DefaultRules
        ),
    OverrideRules =
        % Remove the rules that the user wants to "disable" and after that,
        % remains just the rules the user wants to override.
        lists:filtermap(
            fun(Tuple) ->
                Rule = elvis_rule:from_tuple(Tuple),
                case elvis_rule:disabled(Rule) of
                    false ->
                        {true, Rule};
                    true ->
                        false
                end
            end,
            UserRules
        ),

    UnduplicatedRules ++ OverrideRules.

-spec is_rule_override(
    Rule :: elvis_rule:t(),
    UserRules :: [{NS :: module(), Name :: atom()}]
) ->
    boolean().
is_rule_override(Rule, UserRules) ->
    lists:any(
        fun(UserRule) ->
            elvis_rule:same(Rule, elvis_rule:from_tuple(UserRule))
        end,
        UserRules
    ).

validate_config(ElvisConfig) ->
    do_validate({config, ElvisConfig}).

get_elvis_opt(OptName, Elvis) ->
    proplists:get_value(OptName, Elvis, default_for(OptName)).

elvis_control_opts() ->
    [output_format, verbose, no_output, parallel].

do_validate({elvis = Option, Elvis}) ->
    case check_flag(validation_started(Option)) of
        true ->
            ok;
        _ ->
            maybe
                ok = flag(validation_started(Option)),
                ok ?= is_nonempty_list(elvis, Elvis),
                ok ?=
                    proplist_keys_are_in(
                        'elvis', Elvis, elvis_control_opts() ++ [rulesets, config]
                    ),
                OutputFormat = get_elvis_opt(output_format, Elvis),
                ok ?= is_one_of('elvis.output_format', OutputFormat, [colors, plain, parsable]),
                Verbose = get_elvis_opt(verbose, Elvis),
                ok ?= is_boolean('elvis.verbose', Verbose),
                NoOutput = get_elvis_opt(no_output, Elvis),
                ok ?= is_boolean('elvis.no_output', NoOutput),
                Parallel = get_elvis_opt(parallel, Elvis),
                ok ?= is_pos_integer('elvis.parallel', Parallel),
                CustomRulesets = get_elvis_opt(rulesets, Elvis),
                ok ?= are_valid_rulesets('elvis.rulesets', CustomRulesets),
                ElvisConfig = get_elvis_opt(config, Elvis),
                ok ?= is_valid_config('elvis.config', maps:keys(CustomRulesets), ElvisConfig)
            else
                {error, FormatData} ->
                    do_validate_throw(FormatData)
            end
    end;
do_validate({config = Option, ElvisConfig}) ->
    case check_flag(validation_started(Option)) of
        true ->
            ok;
        _ ->
            maybe
                ok ?= is_valid_config('elvis.config', elvis_ruleset:custom_names(), ElvisConfig)
            else
                {error, FormatData} ->
                    do_validate_throw(FormatData)
            end
    end.

-spec do_validate_throw(_) -> no_return().
do_validate_throw(FormatData) ->
    {Format, Data} =
        case is_list(FormatData) of
            false ->
                FormatData;
            true ->
                % Get only first result, for now
                % If we wanna be smarter we need to start concatenating readable strings
                [{Format0, Data0} | _] = FormatData,
                {Format0, Data0}
        end,
    throw({invalid_config, io_lib:format(Format, Data)}).

is_nonempty_list(What, List) when not is_list(List) orelse List =:= [] ->
    {error, {"'~s' is expected to be a non-empty list.", [What]}};
is_nonempty_list(_What, _List) ->
    ok.

proplist_keys_are_in(What, List, Keys) ->
    Filtered = [Element || {Element, _} <- List, not lists:member(Element, Keys)],
    case Filtered of
        [] ->
            ok;
        _ ->
            {error, {"in '~s', the following keys are unknown: ~p.", [What, Filtered]}}
    end.

is_one_of(What, Value, Possibilities) ->
    case lists:member(Value, Possibilities) of
        true ->
            ok;
        _ ->
            {error, {"'~s' is expected to be one of the following: ~p.", [What, Possibilities]}}
    end.

is_boolean(_What, Value) when is_boolean(Value) ->
    ok;
is_boolean(What, _Value) ->
    {error, {"'~s' is expected to be a boolean.", [What]}}.

is_pos_integer(_What, Value) when is_integer(Value) andalso Value > 0 ->
    ok;
is_pos_integer(What, _Value) ->
    {error, {"'~s' is expected to be a positive integer.", [What]}}.

are_valid_rulesets(What, CustomRulesets) ->
    maybe
        ok ?= is_map(What, CustomRulesets),
        ok ?= all_map_keys_are_atoms(What, CustomRulesets),
        ok ?= all_custom_rulesets_have_valid_rules(What, CustomRulesets),
        ok ?= no_default_ruleset_override(What, CustomRulesets)
    else
        {error, FormatData} ->
            {error, FormatData}
    end.

is_map(_What, Value) when is_map(Value) ->
    ok;
is_map(What, _Value) ->
    {error, {"'~s' is expected to be a map.", [What]}}.

all_map_keys_are_atoms(What, Map) ->
    Filtered = [Key || Key <- maps:keys(Map), not is_atom(Key)],
    case Filtered of
        [] ->
            ok;
        _ ->
            {error, {"in '~s', keys are expected to be atoms.", [What]}}
    end.

all_custom_rulesets_have_valid_rules(What, CustomRulesets) ->
    AccOut = maps:fold(
        fun(CustomRuleset, RuleTuples, AccInO) ->
            lists:foldl(
                fun(RuleTuple, AccInI) ->
                    case elvis_rule:is_valid_from_tuple(RuleTuple) of
                        {true, _Rule} ->
                            AccInI;
                        {false, ValidError} ->
                            [
                                {"in '~s', in ruleset '~s', " ++ ValidError, [What, CustomRuleset]}
                                | AccInI
                            ]
                    end
                end,
                AccInO,
                RuleTuples
            )
        end,
        [],
        CustomRulesets
    ),
    case AccOut of
        [] ->
            ok;
        _ ->
            {error, lists:reverse(AccOut)}
    end.

no_default_ruleset_override(What, CustomRulesets) ->
    Filtered = [
        CustomRuleset
     || CustomRuleset <- maps:keys(CustomRulesets), elvis_ruleset:is_defined(CustomRuleset)
    ],
    case Filtered of
        [] ->
            ok;
        _ ->
            {error,
                {
                    "in '~s', the following rulesets are not expected to be "
                    "named after a default ruleset: ~p.",
                    [
                        What, Filtered
                    ]
                }}
    end.

is_valid_config(What, CustomRulesetNames, Configset0) ->
    Configset = wrap_in_list(Configset0),
    maybe
        ok ?= is_nonempty_list(What, Configset),
        ok ?= all_configs_are_valid(What, CustomRulesetNames, Configset)
    else
        {error, FormatData} ->
            {error, FormatData}
    end.

wrap_in_list(Term) when is_list(Term) ->
    Term;
wrap_in_list(Term) ->
    [Term].

all_configs_are_valid(What, CustomRulesetNames, Configset) ->
    {_PosNumber, ValidErrors} = lists:foldl(
        fun(Config, {PosNumber, AccIn}) ->
            AccOut =
                case config_is_valid(CustomRulesetNames, Config) of
                    ok ->
                        AccIn;
                    {error, ValidError} ->
                        [
                            {"in '~s', at list position number ~p, " ++ ValidError, [
                                What, PosNumber
                            ]}
                            | AccIn
                        ]
                end,
            {PosNumber + 1, AccOut}
        end,
        {1, []},
        Configset
    ),
    case ValidErrors of
        [] ->
            ok;
        _ ->
            {error, lists:reverse(ValidErrors)}
    end.

get_config_opt(OptName, Config) ->
    maps:get(OptName, Config, default_for([config, OptName])).

config_is_valid(CustomRulesetNames, Config) ->
    maybe
        ok ?= map_keys_are_in(Config, [dirs, filter, ignore, ruleset, rules]),
        Dirs = get_config_opt(dirs, Config),
        ok ?= is_nonempty_list_of_dirs(dirs, Dirs),
        Filter = get_config_opt(filter, Config),
        ok ?= is_nonempty_string(filter, Filter),
        ok ?= all_dirs_filter_combos_are_valid(Dirs, Filter),
        Ignore = get_config_opt(ignore, Config),
        ok ?= is_list_of_ignorables(ignore, Ignore),
        Ruleset = get_config_opt(ruleset, Config),
        ok ?= defined_ruleset_is_custom_or_default(CustomRulesetNames, Ruleset),
        Rules = get_config_opt(rules, Config),
        ok ?= all_rules_are_valid(rules, Rules),
        ok ?= either_rules_is_nonempty_or_ruleset_is_defined(Rules, Ruleset)
    else
        {error, ValidError} ->
            {error, ValidError}
    end.

map_keys_are_in(Map, Keys) ->
    Filtered = [Key || Key <- maps:keys(Map), not lists:member(Key, Keys)],
    case Filtered of
        [] ->
            ok;
        _ ->
            {error, io_lib:format("the following keys are unknown: ~p.", [Filtered])}
    end.

is_nonempty_list_of_dirs(What, List) when not is_list(List) orelse List =:= [] ->
    {error, io_lib:format("'~s' is expected to be a non-empty list.", [What])};
is_nonempty_list_of_dirs(What, List) ->
    Filtered = [
        Element
     || Element <- List, not io_lib:char_list(Element) orelse not holds_dir(Element)
    ],
    case Filtered of
        [] ->
            ok;
        _ ->
            {error,
                io_lib:format(
                    "in '~s', the following elements are not (or don't contain) directories: ~p.",
                    [What, Filtered]
                )}
    end.

holds_dir(Element) ->
    Dirs = filelib:wildcard(Element),
    Filtered = [Dir || Dir <- Dirs, filelib:is_dir(Dir)],
    Filtered =/= [].

is_nonempty_string(What, String) ->
    case io_lib:char_list(String) andalso length(String) > 0 of
        true ->
            ok;
        _ ->
            {error, io_lib:format("'~s' is expected to be a non-empty string.", [What])}
    end.

all_dirs_filter_combos_are_valid(Dirs, Filter) ->
    AccOut = lists:foldl(
        fun(Dir, AccIn) ->
            case filelib:wildcard(filename:join(Dir, Filter)) of
                [_ | _] ->
                    AccIn;
                _ ->
                    [
                        io_lib:format(
                            "'<dir>' + '<filter>' combo '~s' + '~s' yielded no files to analyse.", [
                            Dir, Filter
                            ]
                        )
                        | AccIn
                    ]
            end
        end,
        [],
        Dirs
    ),
    case AccOut of
        [] ->
            ok;
        _ ->
            {error, lists:reverse(AccOut)}
    end.

is_list_of_ignorables(What, List) when not is_list(List) ->
    {error, io_lib:format("'~s' is expected to be a list.", [What])};
is_list_of_ignorables(What, List) ->
    Filtered = [Element || Element <- List, not elvis_rule:is_ignorable(Element)],
    case Filtered of
        [] ->
            ok;
        _ ->
            {error,
                io_lib:format("in '~s', the following elements are not ignorable: ~p.", [
                    What, Filtered
                ])}
    end.

defined_ruleset_is_custom_or_default(_CustomRulesetNames, undefined = _Ruleset) ->
    ok;
defined_ruleset_is_custom_or_default(CustomRulesetNames, Ruleset) ->
    case lists:member(Ruleset, CustomRulesetNames) orelse elvis_ruleset:is_defined(Ruleset) of
        true ->
            ok;
        _ ->
            {error,
                io_lib:format("'~s' is expected to be either a custom or a default ruleset.", [
                    Ruleset
                ])}
    end.

all_rules_are_valid(What, RuleTuples) when not is_list(RuleTuples) ->
    {error, io_lib:format("'~s' is expected to be a list.", [What])};
all_rules_are_valid(What, RuleTuples) ->
    AccOut = lists:foldl(
        fun(RuleTuple, AccInI) ->
            case elvis_rule:is_valid_from_tuple(RuleTuple) of
                {true, Rule} ->
                    check_rule_for_options(Rule, AccInI);
                {false, ValidError} ->
                    [io_lib:format("in '~s', " ++ ValidError, [What]) | AccInI]
            end
        end,
        [],
        RuleTuples
    ),
    case AccOut of
        [] ->
            ok;
        _ ->
            {error, lists:reverse(AccOut)}
    end.

either_rules_is_nonempty_or_ruleset_is_defined([_ | _] = _Rules, _Ruleset) ->
    ok;
either_rules_is_nonempty_or_ruleset_is_defined(_Rules, Ruleset) when Ruleset =/= undefined ->
    ok;
either_rules_is_nonempty_or_ruleset_is_defined(_Rules, _Ruleset) ->
    io_lib:format("either rules or ruleset is expected to be defined.", []).

check_rule_for_options(Rule, AccInI) ->
    case elvis_rule:defkeys(Rule) of
        [] ->
            % No further validation possible.
            AccInI;
        DefKeysInput ->
            NS = elvis_rule:ns(Rule),
            Name = elvis_rule:name(Rule),
            % Bypass new/ constraints.
            DefKeys = maps:keys(NS:default(Name)) ++ [ignore],
            case DefKeysInput -- DefKeys of
                [] ->
                    AccInI;
                Extra ->
                    [
                        io_lib:format(
                            "in rule ~p/~p, the following options are unknown: ~p.",
                            [NS, Name, Extra]
                        ),
                        AccInI
                    ]
            end
    end.

flag({_Option, _What} = Obj) ->
    _ = create_table(elvis_config),
    true = ets:insert(elvis_config, Obj),
    ok.

check_flag({Option, _What} = Obj) ->
    table_exists() andalso ets:lookup(elvis_config, Option) =:= [Obj].

validation_started(Option) ->
    {Option, validation_started}.

create_table(Table) ->
    case table_exists() of
        false ->
            _ = ets:new(Table, [public, named_table]);
        _ ->
            ok
    end.

table_exists() ->
    ets:info(elvis_config) =/= undefined.
