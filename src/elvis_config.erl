-module(elvis_config).

-feature(maybe_expr, enable).

-elvis([
    {elvis_style, abc_size, #{ignore => [{elvis_config, default_for, 1}]}},
    {elvis_style, code_complexity, #{ignore => [{elvis_config, default_for, 1}]}}
]).

-export([from_rebar/1, from_file/1, validate/2, default/0]).
%% Getters
-export([files/1, rules/1, ruleset/1]).
%% Files
-export([resolve_files/1, resolve_files/2, apply_to_files/2]).
%% Rules
-export([merge_rules/2]).

%% Options
-export([config/0, output_format/0, verbose/0, no_output/0, parallel/0, warnings_as_errors/0]).
-export([
    set_output_format/1, set_verbose/1, set_no_output/1, set_parallel/1, set_warnings_as_errors/1
]).

% Corresponds to each config map in the 'config' key.
-opaque t() ::
    #{
        files := [nonempty_string()],
        ruleset := atom(),
        rules => [tuple()],
        resolved_files => dynamic(),
        ignore => [string()]
    }.
-export_type([t/0]).

-type output_format() :: plain | colors | parsable.
-export_type([output_format/0]).

% API exports, not consumed locally.
-ignore_xref([from_rebar/1, from_file/1, default/0, resolve_files/2, apply_to_files/2]).
-ignore_xref([
    set_output_format/1, set_verbose/1, set_no_output/1, set_parallel/1, set_warnings_as_errors/1
]).

-ifdef(TEST).
-export([reset_validation/0]).
-export([inject_ignore/2]).
-export([file_globs/1]).
-export([ignore/1]).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

from_static(config, {Type, Config}) ->
    inject_ignore(do_from_static(config, {Type, Config}), git_check_ignore());
from_static(Key, {Type, Config}) ->
    _ = elvis_utils:debug("fetching key '~s' from '~s' configuration", [Key, Type]),
    do_from_static(Key, {Type, Config}).

do_from_static(Key, {Type, Config}) ->
    _ = elvis_utils:debug("fetching key '~s' from '~s' configuration", [Key, Type]),
    case proplists:get_value(Key, Config) of
        undefined ->
            _ = elvis_utils:debug(
                "no value for key '~s' found in '~s' configuration; going with default", [
                    Key, Type
                ]
            ),
            default(Key);
        Value ->
            _ = elvis_utils:debug("value for key '~s' found in '~s' configuration", [Key, Type]),
            Value
    end.

-spec config() -> [t()] | {error, Message :: string()}.
config() ->
    for(config).

-spec output_format() -> output_format() | {error, Message :: string()}.
output_format() ->
    for(output_format).

-spec verbose() -> boolean() | {error, Message :: string()}.
verbose() ->
    for(verbose).

-spec no_output() -> boolean() | {error, Message :: string()}.
no_output() ->
    for(no_output).

-spec parallel() -> pos_integer() | {error, Message :: string()}.
parallel() ->
    for(parallel).

-spec warnings_as_errors() -> boolean() | {error, Message :: string()}.
warnings_as_errors() ->
    for(warnings_as_errors).

-spec set_output_format(output_format()) -> ok.
set_output_format(OutputFormat) ->
    set_env(output_format, OutputFormat).

-spec set_verbose(boolean()) -> ok.
set_verbose(Verbose) ->
    set_env(verbose, Verbose).

-spec set_no_output(boolean()) -> ok.
set_no_output(NoOutput) ->
    set_env(no_output, NoOutput).

-spec set_parallel(pos_integer()) -> ok.
set_parallel(Parallel) ->
    set_env(parallel, Parallel).

-spec set_warnings_as_errors(boolean()) -> ok.
set_warnings_as_errors(WarningsAsErrors) ->
    set_env(warnings_as_errors, WarningsAsErrors).

set_env(Key, Value) ->
    application:set_env(elvis_core, Key, Value).

default(Key) ->
    case application:get_env(elvis_core, Key) of
        undefined ->
            _ = elvis_utils:debug(
                "no value for key '~s' found in application environment; going with default",
                [Key]
            ),
            default_for(Key);
        {ok, Value} ->
            _ = elvis_utils:debug("value for key '~s' (~p) found in application environment", [
                Key, Value
            ]),
            Value
    end.

for(Key) ->
    maybe
        AppDefault = default_for(app),
        {ok, ElvisConfig} ?= consult_elvis_config("elvis.config"),
        {ok, AppConfig} ?=
            case ElvisConfig of
                AppDefault ->
                    % This might happen whether we fail to parse the file or it actually is []
                    _ = elvis_utils:debug(
                        "elvis.config is unusable; falling back to rebar.config", []
                    ),
                    consult_rebar_config("rebar.config");
                AppConfig0 ->
                    {ok, AppConfig0}
            end,
        from_static(Key, {app, AppConfig})
    else
        {error, _} = Error -> Error
    end.

consult_elvis_config(File) ->
    case file:consult(File) of
        {ok, [AppConfig]} when is_list(AppConfig) ->
            _ = elvis_utils:debug("elvis.config is consultable; using it", []),
            {ok, AppConfig};
        {error, {Line, Mod, Term}} ->
            {error,
                lists:flatten(
                    io_lib:format("elvis.config is unconsultable: ~p, ~p, ~p", [Line, Mod, Term])
                )};
        _ ->
            _ = elvis_utils:debug("elvis.config is unconsultable", []),
            {ok, default_for(app)}
    end.

consult_rebar_config(File) ->
    case file:consult(File) of
        {ok, AppConfig} when is_list(AppConfig) ->
            _ = elvis_utils:debug("rebar.config is consultable; using it", []),
            {ok, from_static(elvis, {'rebar.config', AppConfig})};
        {error, {Line, Mod, Term}} ->
            {error,
                lists:flatten(
                    io_lib:format("rebar.config is unconsultable: ~p, ~p, ~p", [Line, Mod, Term])
                )};
        _ ->
            _ = elvis_utils:debug("rebar.config is unconsultable", []),
            {ok, default_for('rebar.config')}
    end.

-spec from_rebar(File :: string()) -> [t()] | {error, Message :: string()}.
from_rebar(File) ->
    maybe
        {ok, AppConfig} ?= consult_rebar_config(File),
        fetch_elvis_config_from(AppConfig, File)
    else
        {error, _} = Error -> Error
    end.

-spec from_file(File :: string()) -> [t()] | {error, Message :: string()}.
from_file(File) ->
    maybe
        {ok, AppConfig} ?= consult_elvis_config(File),
        fetch_elvis_config_from(AppConfig, File)
    else
        {error, _} = Error -> Error
    end.

fetch_elvis_config_from(AppConfig, File) ->
    try do_validate({app, AppConfig, File}) of
        ok ->
            from_static(config, {app, AppConfig})
    catch
        {invalid_config, Message} ->
            {error, lists:flatten(Message)}
    end.

default_for(app) ->
    % This is the top-level element
    [];
default_for(elvis) ->
    [];
default_for('rebar.config') ->
    [{elvis, default_for(elvis)}];
default_for(config) ->
    [];
default_for(output_format) ->
    colors;
default_for(verbose) ->
    false;
default_for(no_output) ->
    false;
default_for(parallel) ->
    erlang:system_info(schedulers_online);
default_for(warnings_as_errors) ->
    true;
default_for(rulesets) ->
    #{};
default_for([config, files]) ->
    [];
default_for([config, ignore]) ->
    [];
default_for([config, ruleset]) ->
    undefined;
default_for([config, rules]) ->
    [].

-spec default() -> [t()].
default() ->
    [
        #{
            files => ["apps/**/src/*.erl", "src/**/*.erl"],
            ruleset => erl_files
        },
        #{
            files => ["apps/**/src/*.hrl", "apps/**/include/*.hrl", "src/*.hrl", "include/*.hrl"],
            ruleset => hrl_files
        },
        #{files => ["rebar.config"], ruleset => rebar_config},
        #{files => [".gitignore"], ruleset => gitignore}
    ].

-ifdef(TEST).
-spec file_globs(t()) -> [string()].
file_globs(#{files := Globs}) when is_list(Globs) ->
    Globs;
file_globs(#{}) ->
    [].
-endif.

-spec ignore([t()] | t()) -> [string()].
ignore(Config) when is_list(Config) ->
    lists:flatmap(fun ignore/1, Config);
ignore(#{ignore := Ignore}) ->
    lists:map(fun ignore_to_regexp/1, Ignore);
ignore(#{}) ->
    [].

-spec files(RuleGroup :: [t()] | t()) -> [elvis_file:t()].
files(RuleGroup) when is_list(RuleGroup) ->
    lists:map(fun files/1, RuleGroup);
files(#{resolved_files := Files}) ->
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
    filter_removed_rules(Rules);
rules(#{ruleset := Ruleset}) ->
    elvis_ruleset:rules(Ruleset);
rules(#{}) ->
    [].

-spec ruleset(t()) -> Ruleset :: atom() | undefined.
ruleset(ElvisConfig) ->
    maps:get(ruleset, ElvisConfig, undefined).

%% @doc Takes a configuration and a list of files, filtering some
%%      of them according to the 'files' globs and ignore list.
%% @end
%% resolve_files/2 with a [t()] type is used in elvis project
-spec resolve_files(Config :: [t()] | t(), Files :: [elvis_file:t()]) ->
    [t()] | t().
resolve_files(Config, Files) when is_list(Config) ->
    Fun = fun(RuleGroup) -> resolve_files(RuleGroup, Files) end,
    lists:map(Fun, Config);
resolve_files(RuleGroup, Files) when is_map(RuleGroup) ->
    FileGlobs = maps:get(files, RuleGroup, []),
    Ignore = ignore(RuleGroup),
    FilteredFiles = elvis_file:filter_files(Files, FileGlobs, Ignore),
    RuleGroup#{resolved_files => FilteredFiles}.

%% @doc Takes a configuration and finds all files according to its 'files' globs.
%% @end
-spec resolve_files(t()) -> t().
resolve_files(#{resolved_files := _} = RuleGroup) ->
    RuleGroup;
resolve_files(#{files := FileGlobs} = RuleGroup) ->
    Ignore = ignore(RuleGroup),
    FoundFiles = elvis_file:find_files(FileGlobs),
    FilteredFiles = filter_by_ignore(FoundFiles, Ignore),
    RuleGroup#{resolved_files => FilteredFiles}.

filter_by_ignore(Files, Ignore) ->
    lists:filter(
        fun(#{path := Path}) ->
            not lists:any(
                fun(Regex) -> match =:= re:run(Path, Regex, [{capture, none}]) end,
                Ignore
            )
        end,
        Files
    ).

%% @doc Takes a function and configuration and applies the function to all
%%      file in the configuration.
%% @end
-spec apply_to_files(Fun :: fun(), Config :: [t()] | t()) ->
    [t()] | t().
apply_to_files(Fun, Config) when is_list(Config) ->
    ApplyFun = fun(RuleGroup) -> apply_to_files(Fun, RuleGroup) end,
    lists:map(ApplyFun, Config);
apply_to_files(Fun, #{resolved_files := Files} = RuleGroup) ->
    NewFiles = lists:map(Fun, Files),
    RuleGroup#{resolved_files => NewFiles}.

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
merge_rules(UserRules0, DefaultRules) ->
    UserRules = filter_removed_rules(UserRules0),
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

filter_removed_rules(RuleTuples) ->
    lists:filter(
        fun(RuleTuple) ->
            case elvis_rule:is_valid_from_tuple(RuleTuple) of
                {removed, _} ->
                    false;
                _ ->
                    true
            end
        end,
        RuleTuples
    ).

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

-spec validate(term(), undefined | string()) -> ok | {error, Message :: string()}.
validate(ElvisConfig, File) ->
    try
        do_validate({config, ElvisConfig, File})
    catch
        {invalid_config, Message} ->
            {error, lists:flatten(Message)}
    end.

get_elvis_opt(OptName, Elvis) ->
    proplists:get_value(OptName, Elvis, default_for(OptName)).

elvis_control_opts() ->
    [output_format, verbose, no_output, parallel, warnings_as_errors].

do_validate({app = _Option, Elvis, File}) ->
    maybe
        ok ?= is_nonempty_list("file '" ++ File ++ "'", Elvis),
        ok ?=
            proplist_keys_are_in(
                File, Elvis, elvis_control_opts() ++ [rulesets, config]
            ),
        OutputFormat = get_elvis_opt(output_format, Elvis),
        ok ?= is_one_of(output_format, OutputFormat, [colors, plain, parsable], File),
        Verbose = get_elvis_opt(verbose, Elvis),
        ok ?= is_boolean(verbose, Verbose, File),
        NoOutput = get_elvis_opt(no_output, Elvis),
        ok ?= is_boolean(no_output, NoOutput, File),
        Parallel = get_elvis_opt(parallel, Elvis),
        ok ?= is_pos_integer(parallel, Parallel, File),
        WarningsAsErrors = get_elvis_opt(warnings_as_errors, Elvis),
        ok ?= is_boolean(warnings_as_errors, WarningsAsErrors, File),
        CustomRulesets = get_elvis_opt(rulesets, Elvis),
        ok ?= are_valid_rulesets(rulesets, CustomRulesets, File),
        Configset = get_elvis_opt(config, Elvis),
        ok ?= is_valid_config(config, maps:keys(CustomRulesets), Configset, File),
        _ = elvis_ruleset:load_custom(from_static(rulesets, {app, Elvis}))
    else
        {v, true} ->
            ok;
        {error, FormatData} ->
            do_validate_throw(FormatData)
    end;
do_validate({config = _Option, ElvisConfig, File}) ->
    maybe
        ok ?= is_valid_config(config, elvis_ruleset:custom_names(), ElvisConfig, File)
    else
        {v, true} ->
            ok;
        {error, FormatData} ->
            do_validate_throw(FormatData)
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
    throw({invalid_config, lists:flatten(io_lib:format(Format, Data))}).

is_nonempty_list(What, List) when not is_list(List) orelse List =:= [] ->
    {error, {"~s is expected to exist and be a non-empty list.", [What]}};
is_nonempty_list(_What, _List) ->
    ok.

proplist_keys_are_in(What, List, Keys) ->
    Filtered = [Element || {Element, _} <- List, not lists:member(Element, Keys)],
    case Filtered of
        [] ->
            ok;
        _ ->
            {error,
                {"in file '~s', the following keys are unknown: ~s.", [
                    What, elvis_utils:list_to_str(Filtered)
                ]}}
    end.

is_one_of(What, Value, Possibilities, File) ->
    case lists:member(Value, Possibilities) of
        true ->
            ok;
        _ ->
            {error,
                {"in file '~s', key '~s' is expected to be one of the following: ~s.", [
                    File, What, elvis_utils:list_to_str(Possibilities)
                ]}}
    end.

is_boolean(_What, Value, _File) when is_boolean(Value) ->
    ok;
is_boolean(What, _Value, File) ->
    {error, {"in file '~s', key '~s' is expected to be a boolean.", [File, What]}}.

is_pos_integer(_What, Value, _File) when is_integer(Value) andalso Value > 0 ->
    ok;
is_pos_integer(What, _Value, File) ->
    {error, {"in file '~s', key '~s' is expected to be a positive integer.", [File, What]}}.

are_valid_rulesets(What, CustomRulesets, File) ->
    maybe
        ok ?= is_map(What, CustomRulesets, File),
        ok ?= all_map_keys_are_atoms(What, CustomRulesets, File),
        ok ?= all_custom_rulesets_have_valid_rules(What, CustomRulesets, File),
        ok ?= no_default_ruleset_override(What, CustomRulesets, File)
    else
        {error, FormatData} ->
            {error, FormatData}
    end.

is_map(_What, Value, _File) when is_map(Value) ->
    ok;
is_map(What, _Value, File) ->
    {error, {"in file '~s', key '~s' is expected to be a map.", [File, What]}}.

all_map_keys_are_atoms(What, Map, File) ->
    Filtered = [Key || Key <- maps:keys(Map), not is_atom(Key)],
    case Filtered of
        [] ->
            ok;
        _ ->
            {error, {"in file '~s', key '~s', map keys are expected to be atoms.", [File, What]}}
    end.

all_custom_rulesets_have_valid_rules(What, CustomRulesets, File) ->
    AccOut = maps:fold(
        fun(CustomRuleset, RuleTuples, AccInO) ->
            lists:foldl(
                fun(RuleTuple, AccInI) ->
                    case elvis_rule:is_valid_from_tuple(RuleTuple) of
                        {true, _Rule} ->
                            AccInI;
                        {removed, Msg} ->
                            _ = elvis_utils:warn("~s Skipping.", [Msg]),
                            AccInI;
                        {false, ValidError} ->
                            [
                                {"in file '~s', map key '~s', in ruleset '~s', " ++ ValidError, [
                                    File, What, CustomRuleset
                                ]}
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

no_default_ruleset_override(What, CustomRulesets, File) ->
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
                    "in file '~s', key '~s', the following entries are not expected to be "
                    "named after a default ruleset: ~s.",
                    [
                        File, What, elvis_utils:list_to_str(Filtered)
                    ]
                }}
    end.

is_valid_config(What0, CustomRulesetNames, Configset0, File0) ->
    maybe
        What =
            case File0 of
                undefined ->
                    "key '" ++ atom_to_list(What0) ++ "'";
                _ ->
                    "in file '" ++ File0 ++ "', key '" ++ atom_to_list(What0) ++ "'"
            end,
        ok ?= is_nonempty_list(What, Configset0),
        Configset = wrap_in_list(Configset0),
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
                            {"~s, at list position number ~w, " ++ ValidError, [
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

get_config_opt(OptName, Config, false = _Compulsory) ->
    {ok, maps:get(OptName, Config, default_for([config, OptName]))};
get_config_opt(OptName, Config, true = _Compulsory) ->
    case maps:get(OptName, Config, undefined) of
        undefined ->
            {error, io_lib:format("is missing compulsory map key '~s'.", [OptName])};
        _GetWithDefault ->
            get_config_opt(OptName, Config, false)
    end.

config_is_valid(CustomRulesetNames, Config) ->
    maybe
        ok ?= map_keys_are_in(Config, [files, ignore, ruleset, rules]),
        {ok, FileGlobs} ?= get_config_opt(files, Config, true),
        ok ?= all_files_globs_are_valid(FileGlobs),
        {ok, Ignore} ?= get_config_opt(ignore, Config, false),
        ok ?= is_list_of_ignorables(ignore, Ignore),
        {ok, Ruleset} ?= get_config_opt(ruleset, Config, false),
        ok ?= defined_ruleset_is_custom_or_default(CustomRulesetNames, Ruleset),
        {ok, Rules} ?= get_config_opt(rules, Config, false),
        ok ?= all_rules_are_valid(rules, Rules),
        ok ?= either_rules_is_nonempty_or_ruleset_is_defined(Rules, Ruleset)
    else
        {error, {Format, Args}} ->
            {error, io_lib:format(Format, Args)};
        {error, ValidError} ->
            {error, ValidError}
    end.

map_keys_are_in(Map, _Keys) when not is_map(Map) ->
    {error, "element is expected to be a map."};
map_keys_are_in(Map, Keys) ->
    Filtered = [Key || Key <- maps:keys(Map), not lists:member(Key, Keys)],
    case Filtered of
        [] ->
            ok;
        _ ->
            {error,
                io_lib:format("the following keys are unknown: ~s.", [
                    elvis_utils:list_to_str(Filtered)
                ])}
    end.

all_files_globs_are_valid(FileGlobs) when not is_list(FileGlobs) orelse FileGlobs =:= [] ->
    {error, "'files' is expected to be a non-empty list."};
all_files_globs_are_valid(FileGlobs) ->
    case lists:all(fun(G) -> is_list(G) andalso G =/= [] end, FileGlobs) of
        true ->
            case lists:any(fun(G) -> filelib:wildcard(G) =/= [] end, FileGlobs) of
                true ->
                    ok;
                false ->
                    {error,
                        io_lib:format("yielded no files to analyse in [\"~s\"].", [
                            lists:join("\", \"", FileGlobs)
                        ])}
            end;
        false ->
            {error, "'files' is expected to be a non-empty list of non-empty strings."}
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
                io_lib:format("in '~s', the following elements are not ignorable: ~s.", [
                    What, elvis_utils:list_to_str(Filtered)
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
                {removed, Msg} ->
                    _ = elvis_utils:warn("~s Skipping.", [Msg]),
                    AccInI;
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
    {error, "either 'rules' is a non-empty list or 'ruleset' is defined."}.

check_rule_for_options(Rule, AccInI) ->
    case elvis_rule:defkeys(Rule) of
        [] ->
            % No further validation possible.
            AccInI;
        DefKeysInput ->
            NS = elvis_rule:ns(Rule),
            Name = elvis_rule:name(Rule),
            % Bypass new/ constraints.
            DefKeys = [ignore | maps:keys(NS:default(Name))],
            case DefKeysInput -- DefKeys of
                [] ->
                    AccInI;
                Extra ->
                    [
                        io_lib:format(
                            "in rule ~w/~w, the following options are unknown: ~s.",
                            [NS, Name, elvis_utils:list_to_str(Extra)]
                        ),
                        AccInI
                    ]
            end
    end.

-spec inject_ignore([t()], [string()]) -> [t()].
inject_ignore(Configs, GitIgnored) ->
    [inject_gitignore(C, GitIgnored) || C <- Configs].

inject_gitignore(#{ignore := Ignore} = Config, GitIgnored) ->
    Config#{ignore => Ignore ++ GitIgnored};
inject_gitignore(Config, GitIgnored) ->
    Config#{ignore => GitIgnored}.

git_check_ignore() ->
    maybe
        GitExecutable = os:find_executable("git"),
        true ?= is_list(GitExecutable),
        RevParse = os:cmd("git rev-parse --is-inside-work-tree 2>&1"),
        true ?= lists:prefix("true", RevParse),
        GitCheckIgnore = do_git_check_ignore(),
        string:lexemes(GitCheckIgnore, "\r\n")
    else
        _ -> []
    end.

do_git_check_ignore() ->
    case os:type() of
        {win32, _} ->
            os:cmd("git check-ignore '*'");
        _ ->
            os:cmd("git check-ignore **/*")
    end.

-ifdef(TEST).
reset_validation() ->
    [persistent_term:erase(K) || {{elvis_config_validation, _} = K, _} <- persistent_term:get()],
    ok.
-endif.
