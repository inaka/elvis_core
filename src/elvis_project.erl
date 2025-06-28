-module(elvis_project).

-behaviour(elvis_ruleset).
-export([default/1]).

-export([no_branch_deps/1, protocol_for_deps/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(RuleName :: atom()) -> elvis_core:rule_config().
default(no_branch_deps) ->
    #{ignore => []};
default(protocol_for_deps) ->
    #{ignore => [], regex => "^(https://|git://|\\d+(\\.\\d+)*)"}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

protocol_for_deps({_Ruleset, _Config, Target, _RuleConfig} = RuleCfg) ->
    IgnoreDeps = elvis_ruleset:option(ignore, RuleCfg, ?FUNCTION_NAME),
    Regex = elvis_ruleset:option(regex, RuleCfg, ?FUNCTION_NAME),
    Deps = get_deps(Target),
    NoHexDeps = lists:filter(fun(Dep) -> not is_hex_dep(Dep) end, Deps),
    BadDeps = lists:filter(fun(Dep) -> is_not_git_dep(Dep, Regex) end, NoHexDeps),
    lists:filtermap(
        fun(Line) ->
            AppName = appname_from_line(Line),

            case lists:member(AppName, IgnoreDeps) of
                true ->
                    false;
                false ->
                    {true,
                        elvis_result:new_item(
                            "Dependency '~s' is not using appropriate protocol; prefer "
                            "respecting regular expression '~s'",
                            [AppName, Regex]
                        )}
            end
        end,
        BadDeps
    ).

appname_from_line({AppName, _}) ->
    AppName;
appname_from_line({AppName, _, _GitInfo}) ->
    AppName;
appname_from_line({AppName, _Vsn, _GitInfo, _Opts}) ->
    AppName.

no_branch_deps({_Ruleset, _Config, Target, _RuleConfig} = RuleCfg) ->
    IgnoreDeps = elvis_ruleset:option(ignore, RuleCfg, ?FUNCTION_NAME),
    Deps = get_deps(Target),
    BadDeps = lists:filter(fun is_branch_dep/1, Deps),
    lists:filtermap(
        fun(Line) ->
            AppName = appname_from_line(Line),

            case lists:member(AppName, IgnoreDeps) of
                true ->
                    false;
                false ->
                    {true, [
                        elvis_result:new_item(
                            "Dependency '~s' uses a branch; prefer a tag or a specific commit",
                            [AppName]
                        )
                    ]}
            end
        end,
        BadDeps
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Rebar

get_deps(File) ->
    {Src, _} = elvis_file:src(File),
    Terms = ktn_code:consult(Src),
    IsDepsTerm =
        fun
            ({deps, _}) ->
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
is_branch_dep({_AppName, {_SCM, _Location, {branch, _}}}) ->
    true;
is_branch_dep({_AppName, {git_subdir, _Url, {branch, _}, _SubDir}}) ->
    true;
%% Specific to plugin rebar_raw_resource
is_branch_dep({AppName, {raw, DepResourceSpecification}}) ->
    is_branch_dep({AppName, DepResourceSpecification});
%% Rebar2
is_branch_dep({_AppName, _Vsn, {_SCM, _Location, {branch, _}}}) ->
    true;
is_branch_dep(_) ->
    false.

is_hex_dep(_AppName) when is_atom(_AppName) ->
    true;
is_hex_dep({_AppName, _Vsn, {pkg, _PackageName}}) when
    is_atom(_AppName), is_list(_Vsn), is_atom(_PackageName)
->
    true;
is_hex_dep({_AppName, {pkg, _OtherName}}) when is_atom(_AppName), is_atom(_OtherName) ->
    true;
is_hex_dep({_AppName, _Vsn}) when is_atom(_AppName), is_list(_Vsn) ->
    true;
is_hex_dep(_) ->
    false.

is_not_git_dep({_AppName, {_SCM, Url, _Branch}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_not_git_dep(
    {_AppName, {git_subdir, Url, {BranchTagOrRefType, _BranchTagOrRef}, _SubDir}},
    Regex
) when
    BranchTagOrRefType =:= branch;
    BranchTagOrRefType =:= tag;
    BranchTagOrRefType =:= ref
->
    nomatch == re:run(Url, Regex, []);
%% Specific to plugin rebar_raw_resource
is_not_git_dep({AppName, {raw, DepResourceSpecification}}, Regex) ->
    is_not_git_dep({AppName, DepResourceSpecification}, Regex);
%% Alternative formats, backwards compatible declarations
is_not_git_dep({_AppName, {_SCM, Url}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_not_git_dep({_AppName, _Vsn, {_SCM, Url}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_not_git_dep({_AppName, _Vsn, {_SCM, Url, _Branch}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_not_git_dep({_AppName, _Vsn, {_SCM, Url, {BranchTagOrRefType, _Branch}}, _Opts}, Regex) when
    BranchTagOrRefType =:= branch;
    BranchTagOrRefType =:= tag;
    BranchTagOrRefType =:= ref
->
    nomatch == re:run(Url, Regex, []).
