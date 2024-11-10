-module(elvis_project).

-export([default/1, no_branch_deps/3, protocol_for_deps/3, old_configuration_format/3]).

-export_type([protocol_for_deps_config/0]).

-define(DEP_BRANCH,
        "Dependency '~s' uses a branch. "
        "Please change this to a tag or specific commit.").
-define(DEP_NO_GIT,
        "Dependency '~s' is not using appropriate protocol, "
        "please change this to something like '~s'").
-define(OLD_CONFIG_FORMAT,
        "The current Elvis configuration file has an outdated format. "
        "Please check Elvis's GitHub repository to find out what the "
        "new format is.").

% These are part of a non-declared "behaviour"
% The reason why we don't try to handle them with different arity is
%  that arguments are ignored in different positions (1 and 3) so that'd
%  probably be messier than to ignore the warning
-hank([{unnecessary_function_arguments,
        [{old_configuration_format, 3}, {no_branch_deps, 3}, {protocol_for_deps, 3}]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(Rule :: atom()) -> DefaultRuleConfig :: term().
default(no_branch_deps) ->
    #{ignore => []};
default(protocol_for_deps) ->
    #{ignore => [], regex => "(https://.*|[0-9]+([.][0-9]+)*)"};
default(old_configuration_format) ->
    #{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type protocol_for_deps_config() :: #{ignore => [module()], regex => string()}.

-spec protocol_for_deps(elvis_config:config(),
                        elvis_file:file(),
                        protocol_for_deps_config()) ->
                           [elvis_result:item()].
protocol_for_deps(_Config, Target, RuleConfig) ->
    IgnoreDeps = option(ignore, RuleConfig, protocol_for_deps),
    Regex = option(regex, RuleConfig, protocol_for_deps),
    Deps = get_deps(Target),
    NoHexDeps = lists:filter(fun(Dep) -> not is_hex_dep(Dep) end, Deps),
    BadDeps = lists:filter(fun(Dep) -> is_not_git_dep(Dep, Regex) end, NoHexDeps),
    lists:flatmap(fun(Line) -> dep_to_result(Line, ?DEP_NO_GIT, {IgnoreDeps, Regex}) end,
                  BadDeps).

-spec no_branch_deps(elvis_config:config(),
                     elvis_file:file(),
                     elvis_style:empty_rule_config()) ->
                        [elvis_result:item()].
no_branch_deps(_Config, Target, RuleConfig) ->
    IgnoreDeps = option(ignore, RuleConfig, no_branch_deps),
    Deps = get_deps(Target),
    BadDeps = lists:filter(fun is_branch_dep/1, Deps),
    lists:flatmap(fun(Line) -> dep_to_result(Line, ?DEP_BRANCH, IgnoreDeps) end, BadDeps).

-spec old_configuration_format(elvis_config:config(),
                               elvis_file:file(),
                               elvis_style:empty_rule_config()) ->
                                  [elvis_result:item()].
old_configuration_format(_Config, Target, _RuleConfig) ->
    {Content, _} = elvis_file:src(Target),
    [AllConfig] = ktn_code:consult(Content),
    case proplists:get_value(elvis, AllConfig) of
        undefined ->
            [];
        ElvisConfig ->
            case is_old_config(ElvisConfig) of
                false ->
                    [];
                true ->
                    [elvis_result:new(item, ?OLD_CONFIG_FORMAT, [])]
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Rebar

%% @private
get_deps(File) ->
    {Src, _} = elvis_file:src(File),
    Terms = ktn_code:consult(Src),
    IsDepsTerm =
        fun ({deps, _}) ->
                true;
            (_) ->
                false
        end,
    case lists:filter(IsDepsTerm, Terms) of
        [] ->
            [];
        [{deps, Deps}] ->
            Deps
    end.

%% Rebar3
%% @private
is_branch_dep({_AppName, {_SCM, _Location, {branch, _}}}) ->
    true;
is_branch_dep({_AppName, {git_subdir, _Url, {branch, _}, _SubDir}}) ->
    true;
is_branch_dep({AppName, {raw, DepResourceSpecification}}) ->
    is_branch_dep({AppName, DepResourceSpecification});
%% Rebar2
%% @private
is_branch_dep({_AppName, _Vsn, {_SCM, _Location, {branch, _}}}) ->
    true;
is_branch_dep(_) ->
    false.

%% @private
is_hex_dep(_AppName) when is_atom(_AppName) ->
    true;
is_hex_dep({_AppName, _Vsn, {pkg, _PackageName}})
    when is_atom(_AppName), is_list(_Vsn), is_atom(_PackageName) ->
    true;
is_hex_dep({_AppName, _Vsn}) when is_atom(_AppName), is_list(_Vsn) ->
    true;
is_hex_dep(_) ->
    false.

%% @private
is_not_git_dep({_AppName, {pkg, _OtherName}}, _Regex) ->
    false;
is_not_git_dep({_AppName, {_SCM, Url, _Branch}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_not_git_dep({AppName, {raw, DepResourceSpecification}}, Regex) ->
    is_not_git_dep({AppName, DepResourceSpecification}, Regex);
is_not_git_dep({_AppName, {_SCM, Url}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_not_git_dep({_AppName, _Vsn, {_SCM, Url}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_not_git_dep({_AppName, _Vsn, {_SCM, Url, _Branch}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_not_git_dep({_AppName, {git_subdir, Url, {branch, _Branch}, _SubDir}}, Regex) ->
    nomatch == re:run(Url, Regex, []).

%% @private
dep_to_result({AppName, _}, Message, {IgnoreDeps, Regex}) ->
    case lists:member(AppName, IgnoreDeps) of
        true ->
            [];
        false ->
            [elvis_result:new(item, Message, [AppName, Regex])]
    end;
dep_to_result({AppName, _}, Message, IgnoreDeps) ->
    case lists:member(AppName, IgnoreDeps) of
        true ->
            [];
        false ->
            [elvis_result:new(item, Message, [AppName])]
    end;
dep_to_result({AppName, _, GitInfo}, Message, {IgnoreDeps, Regex}) ->
    dep_to_result({AppName, GitInfo}, Message, {IgnoreDeps, Regex});
dep_to_result({AppName, _, GitInfo}, Message, IgnoreDeps) ->
    dep_to_result({AppName, GitInfo}, Message, IgnoreDeps).

%% Old config

%% @private
is_old_config(ElvisConfig) ->
    case proplists:get_value(config, ElvisConfig) of
        undefined ->
            false;
        Config when is_map(Config) ->
            true;
        Config when is_list(Config) ->
            SrcDirsIsKey =
                fun(RuleGroup) -> maps:is_key(src_dirs, RuleGroup) orelse exists_old_rule(RuleGroup)
                end,
            lists:filter(SrcDirsIsKey, Config) /= []
    end.

%% @private
exists_old_rule(#{rules := Rules}) ->
    Filter =
        fun ({_, _, Args}) when is_list(Args) ->
                true;
            (_) ->
                false
        end,
    lists:filter(Filter, Rules) /= [];
exists_old_rule(_) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option(OptionName, RuleConfig, Rule) -> OptionValue
    when OptionName :: atom(),
         RuleConfig :: elvis_config:config(),
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
