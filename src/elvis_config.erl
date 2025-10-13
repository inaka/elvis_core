-module(elvis_config).

-feature(maybe_expr, enable).

-export([
    from_rebar/1,
    from_file/1,
    from_application_or_config/2,
    validate/1
]).
%% Geters
-export([dirs/1, ignore/1, filter/1, files/1, rules/1, ruleset/1]).
%% Files
-export([resolve_files/1, resolve_files/2, apply_to_files/2]).
%% Rules
-export([merge_rules/2]).

-export_type([t/0]).

-opaque t() :: map().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec from_rebar(string()) -> [t()].
from_rebar(Path) ->
    case file:consult(Path) of
        {ok, AppConfig} ->
            load(config, load_initial(AppConfig), []);
        {error, Reason} ->
            throw(Reason)
    end.

-spec from_file(string()) -> [t()].
from_file(Path) ->
    from_file(Path, config, []).

-spec from_file(string(), atom(), term()) -> [t()].
from_file(Path, Key, Default) ->
    case file:consult(Path) of
        {ok, [AppConfig]} ->
            load(Key, load_initial(AppConfig), Default);
        {error, {_Line, _Mod, _Term} = Reason} ->
            throw(Reason);
        {error, _Reason} ->
            Default
    end.

-spec from_application_or_config(atom(), term()) -> term().
from_application_or_config(Key, Default) ->
    case application:get_env(elvis_core, Key) of
        {ok, Value} ->
            Value;
        _ ->
            from_file("elvis.config", Key, Default)
    end.

-spec load(atom(), term(), term()) -> [t()].
load(Key, ElvisConfig, Default) ->
    proplists:get_value(Key, ElvisConfig, Default).

-spec load_initial(term()) -> [term()].
load_initial(AppConfig) ->
    ElvisConfig = proplists:get_value(elvis, AppConfig, []),
    RulesetsConfig = proplists:get_value(rulesets, ElvisConfig, #{}),
    elvis_ruleset:set_rulesets(RulesetsConfig),
    ElvisConfig.

-spec validate(Config :: [t()]) -> ok.
validate([]) ->
    throw({invalid_config, empty_config});
validate(Config) ->
    lists:foreach(fun do_validate/1, Config).

do_validate(RuleGroup) ->
    maybe
        ok ?= maybe_missing_dirs(RuleGroup),
        ok ?= maybe_missing_filter(RuleGroup),
        ok ?= maybe_missing_rules(RuleGroup),
        ok ?= maybe_invalid_rules(RuleGroup)
    else
        {error, Error} ->
            throw({invalid_config, Error})
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

-spec ruleset(t()) -> Ruleset :: atom() | undefined.
ruleset(ElvisConfig) ->
    maps:get(ruleset, ElvisConfig, undefined).

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
