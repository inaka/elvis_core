-module(elvis_config).

-export([
         default/0,
         load_file/1,
         load/1,
         validate/1,
         normalize/1,
         %% Geters
         dirs/1,
         ignore/1,
         filter/1,
         files/1,
         rules/1,
         %% Files
         resolve_files/1,
         apply_to_files/2
        ]).

-export_type([
              config/0
             ]).

-type config() :: [map()].

-define(DEFAULT_CONFIG_PATH, "./elvis.config").
-define(DEFAULT_REBAR_CONFIG_PATH, "./rebar.config").
-define(DEFAULT_FILTER, "*.erl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default() -> config().
default() ->
    case file:consult(?DEFAULT_CONFIG_PATH) of
        {ok, [Config]} ->
            load(Config);
        {error, enoent} ->
            case file:consult(?DEFAULT_REBAR_CONFIG_PATH) of
                {ok, Config} ->
                    load(Config);
                {error, enoent} ->
                    Config = application:get_env(elvis, config, []),
                    ensure_config_list(Config);
                {error, Reason} ->
                    throw(Reason)
            end;
        {error, Reason} ->
            throw(Reason)
    end.

-spec load_file(string()) -> config().
load_file(Path) ->
    case file:consult(Path) of
        {ok, [Config]} ->
            load(Config);
        {error, Reason} ->
            throw(Reason)
    end.

-spec load(term()) -> config().
load(AppConfig) ->
    ElvisConfig = proplists:get_value(elvis, AppConfig, []),
    Config =  proplists:get_value(config, ElvisConfig, []),
    ensure_config_list(Config).

ensure_config_list(Config) when is_map(Config) ->
    [Config];
ensure_config_list(Config) ->
    Config.

-spec validate(Config::config()) -> ok.
validate([]) ->
    throw({invalid_config, empty_config});
validate(Config) ->
    lists:foreach(fun do_validate/1, Config).

do_validate(RuleGroup) ->
    case maps:is_key(src_dirs, RuleGroup) or maps:is_key(dirs, RuleGroup) of
        false -> throw({invalid_config, {missing_dirs, RuleGroup}});
        true -> ok
    end,
    case maps:is_key(dirs, RuleGroup) of
        true ->
            case maps:is_key(filter, RuleGroup) of
                false -> throw({invalid_config, {missing_filter, RuleGroup}});
                true -> ok
            end;
        false -> ok
    end,
    case maps:is_key(rules, RuleGroup) orelse maps:is_key(ruleset, RuleGroup) of
        false -> throw({invalid_config, {missing_rules, RuleGroup}});
        true -> ok
    end.

-spec normalize(config()) -> config().
normalize(Config) when is_list(Config) ->
    lists:map(fun do_normalize/1, Config).

%% @private
do_normalize(Config = #{src_dirs := Dirs}) ->
    %% NOTE: Provided for backwards compatibility.
    %% Rename 'src_dirs' key to 'dirs'.
    Config1 = maps:remove(src_dirs, Config),
    Config1#{dirs => Dirs};
do_normalize(Config) ->
    Config.

-spec dirs(Config::config() | map()) -> [string()].
dirs(Config) when is_list(Config) ->
    lists:flatmap(fun dirs/1, Config);
dirs(_RuleGroup = #{dirs := Dirs}) ->
    Dirs;
dirs(#{}) ->
    [].

-spec ignore(config() | map()) -> [string()].
ignore(Config) when is_list(Config) ->
    lists:flatmap(fun ignore/1, Config);
ignore(_RuleGroup = #{ignore := Ignore}) ->
    lists:map(fun ignore_to_regexp/1, Ignore);
ignore(#{}) ->
    [].

-spec filter(config() | map()) -> [string()].
filter(Config) when is_list(Config) ->
    lists:flatmap(fun filter/1, Config);
filter(_RuleGroup = #{filter := Filter}) ->
    Filter;
filter(#{}) ->
    ?DEFAULT_FILTER.

-spec files(map()) -> [elvis_file:file()] | undefined.
files(_RuleGroup = #{files := Files}) ->
    Files;
files(#{}) ->
    undefined.

-spec rules(Rules::config() | map()) -> [string()] | undefined.
rules(Rules) when is_list(Rules) ->
    lists:map(fun rules/1, Rules);
rules(#{rules := UserRules, ruleset := RuleSet}) ->
    DefaultRules = elvis_rulesets:rules(RuleSet),
    merge_rules(UserRules, DefaultRules);
rules(#{rules := Rules}) -> Rules;
rules(#{ruleset := RuleSet}) ->
    elvis_rulesets:rules(RuleSet);
rules(#{}) ->
    undefined.

%% @doc Takes a configuration and a list of files, filtering some
%%      of them according to the 'filter' key, or if not specified
%%      uses '*.erl'.
%% @end
%% resolve_files/2 with a config() type is used in elvis project
-spec resolve_files(config() | map(), Files::[elvis_file:file()]) -> map().
resolve_files(RuleGroup, Files) ->
    Filter = filter(RuleGroup),
    Dirs = dirs(RuleGroup),
    Ignore = ignore(RuleGroup),
    FilteredFiles = elvis_file:filter_files(Files, Dirs, Filter, Ignore),
    RuleGroup#{files => FilteredFiles}.

%% @doc Takes a configuration and finds all files according to its 'dirs'
%%      end  'filter' key, or if not specified uses '*.erl'.
%% @end
-spec resolve_files(map()) -> map().
resolve_files(RuleGroup = #{files := _Files}) ->
    RuleGroup;
resolve_files(RuleGroup = #{dirs := Dirs}) ->
    Filter = filter(RuleGroup),
    Files = elvis_file:find_files(Dirs, Filter),
    resolve_files(RuleGroup, Files).

%% @doc Takes a function and configuration and applies the function to all
%%      file in the configuration.
%% @end
-spec apply_to_files(Fun::fun(), Config::config() | map()) -> config() | map().
apply_to_files(Fun, Config) when is_list(Config) ->
    ApplyFun = fun(RuleGroup) -> apply_to_files(Fun, RuleGroup) end,
    lists:map(ApplyFun, Config);
apply_to_files(Fun, RuleGroup = #{files := Files}) ->
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
-spec merge_rules(UserRules::list(), DefaultRules::list()) -> list().
merge_rules(UserRules, DefaultRules) ->
    UnduplicatedRules =
        % Drops repeated rules
        lists:filter(
            % If any default rule is in UserRules it means the user
            % wants to override the rule.
            fun({FileName, RuleName}) ->
                not is_rule_override(FileName, RuleName, UserRules);
               ({FileName, RuleName, _}) ->
                not is_rule_override(FileName, RuleName, UserRules);
               (_) -> false
            end,
            DefaultRules
        ),
    OverrideRules =
        % Remove the rules that the user wants to "disable" and after that,
        % remains just the rules the user wants to override.
        lists:filter(
            fun({_FileName, _RuleName, OverrideOptions}) ->
                disable /= OverrideOptions;
               (_) -> false end,
            UserRules
        ),
    UnduplicatedRules ++ OverrideRules.

-spec is_rule_override(FileName::atom(), RuleName::atom(), UserRules::list()) ->
    boolean().
is_rule_override(FileName, RuleName, UserRules) ->
    lists:any(
        fun(UserRule) ->
            case UserRule of
                {FileName, RuleName, _} -> true;
                _ -> false
            end
        end,
        UserRules
    ).
