-module(elvis_config).

-feature(maybe_expr, enable).

-export([from_rebar/1, from_file/1]).
%% Geters
-export([dirs/1, ignore/1, filter/1, files/1, rules/1]).
%% Files
-export([resolve_files/1, resolve_files/2, apply_to_files/2]).
%% Rules
-export([merge_rules/2]).

%% Options
-export([config/0, output_format/0, verbose/0, no_output/0, parallel/0]).

-type t() :: map().
-export_type([t/0]).

-type validation_error() :: empty_config.
-export_type([validation_error/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fetch_elvis_config(AppConfig) ->
    ElvisConfig = from_static(elvis, {app, AppConfig}),
    elvis_ruleset:load(from_static(rulesets, {elvis, ElvisConfig})),
    ElvisConfig.

from_static(Key, {Type, Config}) ->
    elvis_utils:output(debug, "fetching key ~p from ~p config.", [Key, Type]),
    case proplists:get_value(Key, Config) of
        undefined ->
            elvis_utils:output(
                debug, "no value for config. key ~p found in ~p config.; going with default", [
                    Key, Type
                ]
            ),
            default(Key);
        Value ->
            elvis_utils:output(debug, "value for config. key ~p found in ~p config.", [Key, Type]),
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

default(Key) ->
    case application:get_env(elvis_core, Key) of
        undefined ->
            elvis_utils:output(
                debug,
                "no value for config. key ~p found in application env.; going with default",
                [Key]
            ),
            default_for(Key);
        {ok, Value} ->
            elvis_utils:output(debug, "value for config. key ~p found in application env.", [Key]),
            Value
    end.

for(Key) ->
    ElvisDefault = default_for(elvis),
    AppConfig =
        case consult_elvis_config("elvis.config") of
            ElvisDefault ->
                % This might happen whether we fail to parse the fail or it actually is []
                elvis_utils:output(
                    debug, "elvis.config unusable; falling back to rebar.config", []
                ),
                consult_rebar_config("rebar.config");
            AppConfig0 ->
                AppConfig0
        end,
    ElvisConfig = fetch_elvis_config(AppConfig),
    from_static(Key, {elvis, ElvisConfig}).

consult_elvis_config(File) ->
    case file:consult(File) of
        {ok, [AppConfig]} ->
            elvis_utils:output(debug, "elvis.config usable; using it", []),
            AppConfig;
        _ ->
            elvis_utils:output(debug, "elvis.config unusable", []),
            default_for(elvis)
    end.

consult_rebar_config(File) ->
    case file:consult(File) of
        {ok, AppConfig0} ->
            elvis_utils:output(debug, "rebar.config usable; using it", []),
            AppConfig0;
        _ ->
            elvis_utils:output(debug, "rebar.config unusable", []),
            default_for(elvis)
    end.

from_rebar(File) ->
    RebarConfig = consult_rebar_config(File),
    ElvisConfig = fetch_elvis_config(RebarConfig),
    from_static(config, {elvis, ElvisConfig}).

from_file(File) ->
    FileConfig = consult_elvis_config(File),
    ElvisConfig = fetch_elvis_config(FileConfig),
    from_static(config, {elvis, ElvisConfig}).

default_for(elvis) ->
    [];
default_for(config) ->
    [
        #{
            dirs => ["apps/*/src/**", "src/**"],
            filter => "*.erl",
            ruleset => erl_files
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
        },
        #{
            dirs => ["."],
            filter => "elvis.config",
            ruleset => elvis_config
        }
    ];
default_for(output_format) ->
    colors;
default_for(verbose) ->
    false;
default_for(no_output) ->
    false;
default_for(parallel) ->
    1;
default_for(rulesets) ->
    #{}.

-spec validate(Config :: [t()]) -> [validation_error()].
validate([]) ->
    [empty_config];
validate(Config) ->
    lists:foreach(fun do_validate/1, Config),
    [].

do_validate(RuleGroup) ->
    maybe
        ok ?= maybe_missing_dirs(RuleGroup),
        ok ?= maybe_missing_filter(RuleGroup),
        ok ?= maybe_missing_rules(RuleGroup),
        ok ?= maybe_invalid_rules(RuleGroup)
    else
        {error, Error} ->
            {error, {invalid_config, Error}}
    end.

maybe_missing_dirs(RuleGroup) ->
    maybe_boolean_wrapper(
        not (maps:is_key(dirs, RuleGroup) andalso not maps:is_key(filter, RuleGroup)), missing_dir
    ).

maybe_missing_filter(RuleGroup) ->
    maybe_boolean_wrapper(
        maps:is_key(dirs, RuleGroup), missing_filter
    ).

maybe_missing_rules(RuleGroup) ->
    maybe_boolean_wrapper(
        maps:is_key(rules, RuleGroup) orelse maps:is_key(ruleset, RuleGroup), missing_rules
    ).

maybe_boolean_wrapper(true, _Flag) -> ok;
maybe_boolean_wrapper(false, Flag) -> {error, Flag}.

maybe_invalid_rules(#{rules := Rules}) ->
    case invalid_rules(Rules) of
        [] -> ok;
        InvalidRules -> {error, {invalid_rules, InvalidRules}}
    end;
maybe_invalid_rules(_) ->
    ok.

invalid_rules(Rules) ->
    lists:filtermap(fun is_invalid_rule/1, Rules).

is_invalid_rule({NS, Rule, _}) ->
    is_invalid_rule({NS, Rule});
is_invalid_rule({NS, Rule}) ->
    maybe
        {module, NS} ?= code:ensure_loaded(NS),
        ExportedRules = erlang:get_module_info(NS, exports),
        case lists:keymember(Rule, 1, ExportedRules) of
            false -> {true, {invalid_rule, {NS, Rule}}};
            _ -> false
        end
    else
        {error, _} ->
            elvis_utils:warn_prn(
                "Invalid module (~p) specified in elvis.config.~n",
                [NS]
            ),
            {true, {invalid_rule, {NS, Rule}}}
    end.

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
    _ =
        case FilteredFiles of
            [] ->
                Ruleset = maps:get(ruleset, RuleGroup, undefined),
                Error =
                    elvis_result:new(
                        warn,
                        "Searching for files in ~p, for ruleset ~p, "
                        "with filter ~p, yielded none. "
                        "Update your configuration",
                        [Dirs, Ruleset, Filter]
                    ),
                ok = elvis_result:print_results([Error]);
            _ ->
                ok
        end,
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
    resolve_files(RuleGroup, Files).

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
                case not is_rule_override(Rule, UserRules) of
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

%flag_validated(Option) ->
%    Table = table(),
%    _ = create_table(Table),
%    Obj = validated_flag(Option),
%    ets:insert(Table, Obj).
%
%is_validated(Option) ->
%    Table = table(),
%    case table_exists(Table) of
%        false ->
%            false;
%        _ ->
%            Obj = validated_flag(Option),
%            ets:lookup(Table, Option) =:= [Obj]
%    end.
%
%validated_flag(Option) ->
%    {Option, validated}.
%
%create_table(Table) ->
%    case table_exists(Table) of
%        false ->
%            _ = ets:new(Table, [public, named_table]);
%        _ ->
%            ok
%    end.
%
%table() ->
%    ?MODULE.
%
%table_exists(Table) ->
%    ets:info(Table) =/= undefined.
%
%-spec default_config() -> [t()].
