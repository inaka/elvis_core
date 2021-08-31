-module(elvis_project).

-export([
         default/1,
         no_deps_master_erlang_mk/3,
         no_deps_master_rebar/3,
         protocol_for_deps_erlang_mk/3,
         git_for_deps_erlang_mk/3,
         protocol_for_deps_rebar/3,
         git_for_deps_rebar/3,
         old_configuration_format/3
        ]).

-define(DEP_MASTER,
        "Dependency '~s' revision is specified 'master', "
        "please change this to a tag, branch or specific "
        "commit.").

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
-hank([{unnecessary_function_arguments, [
            old_configuration_format/3,
            no_deps_master_rebar/3,
            no_deps_master_erlang_mk/3,
            protocol_for_deps_rebar/3,
            protocol_for_deps_erlang_mk/3
       ]}]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(Rule :: atom()) -> DefaultRuleConfig :: term().
default(no_deps_master_erlang_mk) ->
    #{ ignore => []
     };

default(protocol_for_deps_erlang_mk) ->
    #{ ignore => [],
       regex => "(https://.*|[0-9]+([.][0-9]+)*)"
     };

default(no_deps_master_rebar) ->
    #{ ignore => []
     };

default(protocol_for_deps_rebar) ->
    #{ ignore => [],
       regex => "(https://.*|[0-9]+([.][0-9]+)*)"
     };

default(old_configuration_format) ->
    #{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type protocol_for_deps_erlang_mk_config() :: #{ignore => [module()],
                                                regex => string()}.

%% Deprecated
-spec git_for_deps_erlang_mk(elvis_config:config(),
                             elvis_file:file(),
                             protocol_for_deps_erlang_mk_config()) ->
    [elvis_result:item()].
git_for_deps_erlang_mk(Config, Target, RuleConfig) ->
    elvis_utils:error_prn("This rule has been deprecated please use "
                          "'protocol_for_deps_erlang_mk'."),
    protocol_for_deps_erlang_mk(Config, Target, RuleConfig).

-spec protocol_for_deps_erlang_mk(elvis_config:config(),
                             elvis_file:file(),
                             protocol_for_deps_erlang_mk_config()) ->
    [elvis_result:item()].
protocol_for_deps_erlang_mk(_Config, Target, RuleConfig) ->
    IgnoreDeps = option(ignore, RuleConfig, protocol_for_deps_erlang_mk),
    Regex = option(regex, RuleConfig, protocol_for_deps_erlang_mk),
    Deps = get_erlang_mk_deps(Target),
    BadDeps = lists:filter(fun(Dep) -> is_erlang_mk_not_git_dep(Dep, Regex) end,
                           Deps),
    lists:flatmap(
        fun(Line) ->
            erlang_mk_dep_to_result(Line, ?DEP_NO_GIT, {IgnoreDeps, Regex})
        end, BadDeps).

-type protocol_for_deps_rebar_config() :: #{ignore => [module()],
                                            regex => string()}.

-spec git_for_deps_rebar(elvis_config:config(),
                         elvis_file:file(),
                         protocol_for_deps_rebar_config()) ->
    [elvis_result:item()].
git_for_deps_rebar(Config, Target, RuleConfig) ->
    elvis_utils:error_prn("This rule has been deprecated please use "
                          "'protocol_for_deps_rebar'."),
    protocol_for_deps_rebar(Config, Target, RuleConfig).

-spec protocol_for_deps_rebar(elvis_config:config(),
                         elvis_file:file(),
                         protocol_for_deps_rebar_config()) ->
    [elvis_result:item()].
protocol_for_deps_rebar(_Config, Target, RuleConfig) ->
    IgnoreDeps = option(ignore, RuleConfig, protocol_for_deps_rebar),
    Regex = option(regex, RuleConfig, protocol_for_deps_rebar),
    Deps = get_rebar_deps(Target),
    NoHexDeps = lists:filter(fun(Dep) -> not is_rebar_hex_dep(Dep) end,
                             Deps),
    BadDeps = lists:filter(fun(Dep) -> is_rebar_not_git_dep(Dep, Regex) end,
                           NoHexDeps),
    lists:flatmap(
        fun(Line) ->
            rebar_dep_to_result(Line, ?DEP_NO_GIT, {IgnoreDeps, Regex})
        end, BadDeps).

-type no_deps_master_erlang_mk_config() :: #{ignore => [module()]}.

-spec no_deps_master_erlang_mk(elvis_config:config(),
                               elvis_file:file(),
                               no_deps_master_erlang_mk_config()) ->
    [elvis_result:item()].
no_deps_master_erlang_mk(_Config, Target, RuleConfig) ->
    IgnoreDeps = option(ignore, RuleConfig, no_deps_master_erlang_mk),
    Deps = get_erlang_mk_deps(Target),
    BadDeps = lists:filter(fun is_erlang_mk_master_dep/1, Deps),
    lists:flatmap(
        fun(Line) ->
            erlang_mk_dep_to_result(Line, ?DEP_MASTER, IgnoreDeps)
        end, BadDeps).

-type no_deps_master_rebar_config() :: #{ignore => [module()]}.

-spec no_deps_master_rebar(elvis_config:config(),
                           elvis_file:file(),
                           no_deps_master_rebar_config()) ->
    [elvis_result:item()].
no_deps_master_rebar(_Config, Target, RuleConfig) ->
    IgnoreDeps = option(ignore, RuleConfig, no_deps_master_rebar),
    Deps = get_rebar_deps(Target),
    BadDeps = lists:filter(fun is_rebar_master_dep/1, Deps),
    lists:flatmap(
        fun(Line) ->
            rebar_dep_to_result(Line, ?DEP_MASTER, IgnoreDeps)
        end, BadDeps).

-type empty_rule_config() :: #{}.

-spec old_configuration_format(elvis_config:config(),
                               elvis_file:file(),
                               empty_rule_config()) ->
    [elvis_result:item()].
old_configuration_format(_Config, Target, _RuleConfig) ->
    {Content, _} = elvis_file:src(Target),
    [AllConfig] = ktn_code:consult(Content),
    case proplists:get_value(elvis, AllConfig) of
        undefined -> [];
        ElvisConfig ->
            case is_old_config(ElvisConfig) of
                false -> [];
                true ->
                    [elvis_result:new(item, ?OLD_CONFIG_FORMAT, [])]
            end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Rebar

get_rebar_deps(File) ->
    {Src, _} = elvis_file:src(File),
    Terms = ktn_code:consult(Src),
    IsDepsTerm = fun
                     ({deps, _}) -> true;
                     (_) -> false
                 end,
    case lists:filter(IsDepsTerm, Terms) of
        [] -> [];
        [{deps, Deps}] -> Deps
    end.

%% Rebar3
is_rebar_master_dep({_AppName, {_SCM, _Location, "master"}}) ->
    true;
is_rebar_master_dep({_AppName, {_SCM, _Location, {branch, "master"}}}) ->
    true;
is_rebar_master_dep({AppName, {raw, DepResourceSpecification}}) ->
    is_rebar_master_dep({AppName, DepResourceSpecification});
%% Rebar2
is_rebar_master_dep({_AppName, _Vsn, {_SCM, _Location, "master"}}) ->
    true;
is_rebar_master_dep({_AppName, _Vsn, {_SCM, _Location, {branch, "master"}}}) ->
    true;
is_rebar_master_dep(_) ->
    false.
is_rebar_hex_dep(_AppName) when is_atom(_AppName) ->
    true;
is_rebar_hex_dep({_AppName, _Vsn, {pkg, _PackageName}})
  when is_atom(_AppName),
       is_list(_Vsn),
       is_atom(_PackageName) ->
    true;
is_rebar_hex_dep({_AppName, _Vsn})
  when is_atom(_AppName),
       is_list(_Vsn) ->
    true;
is_rebar_hex_dep(_) ->
    false.

is_rebar_not_git_dep({_AppName, {pkg, _OtherName}}, _Regex) ->
    false;
is_rebar_not_git_dep({_AppName, {_SCM, Url, _Branch}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_rebar_not_git_dep({AppName, {raw,  DepResourceSpecification}}, Regex) ->
    is_rebar_not_git_dep({AppName, DepResourceSpecification}, Regex);
is_rebar_not_git_dep({_AppName, {_SCM, Url}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_rebar_not_git_dep({_AppName, _Vsn, {_SCM, Url}}, Regex) ->
    nomatch == re:run(Url, Regex, []);
is_rebar_not_git_dep({_AppName, _Vsn, {_SCM, Url, _Branch}}, Regex) ->
    nomatch == re:run(Url, Regex, []).

rebar_dep_to_result({AppName, _}, Message, {IgnoreDeps, Regex}) ->
  case lists:member(AppName, IgnoreDeps) of
    true -> [];
    false -> [elvis_result:new(item, Message, [AppName, Regex])]
  end;
rebar_dep_to_result({AppName, _}, Message, IgnoreDeps) ->
  case lists:member(AppName, IgnoreDeps) of
    true -> [];
    false -> [elvis_result:new(item, Message, [AppName])]
  end;
rebar_dep_to_result({AppName, _, GitInfo}, Message, {IgnoreDeps, Regex}) ->
  rebar_dep_to_result({AppName, GitInfo}, Message, {IgnoreDeps, Regex});
rebar_dep_to_result({AppName, _, GitInfo}, Message, IgnoreDeps) ->
  rebar_dep_to_result({AppName, GitInfo}, Message, IgnoreDeps).



%%% erlang.mk

is_erlang_mk_master_dep(Line) ->
    case re:run(Line, "master *$", []) of
        nomatch -> false;
        _ -> true
    end.

is_erlang_mk_not_git_dep(Line, Regex) ->
    [_DepName, Dependency] = binary:split(Line, <<"=">>),
    [_Protocol, Url | _] =
        [Part
            || Part <- binary:split(Dependency, <<" ">>, [global, trim])
             , Part /= <<>>],
    nomatch == re:run(Url, Regex, []).

get_erlang_mk_deps(File) ->
    {Src, _} = elvis_file:src(File),
    Lines = elvis_utils:split_all_lines(Src),
    Opts = [{capture, all_but_first, binary}],
    IsDepsLine =
      fun(Line) ->
          case re:run(Line, "dep_([^ ]*)", Opts) of
              nomatch ->
                  false;
              {match, [Name]} ->
                  %% Filter out dep_depname_commit lines
                  binary:longest_common_suffix([Name, <<"_commit">>]) /= 7
          end
      end,
    lists:filter(IsDepsLine, Lines).

erlang_mk_dep_to_result(Line, Message, {IgnoreDeps, Regex}) ->
    Opts = [{capture, all_but_first, binary}],
    {match, [Name]} = re:run(Line, "dep_([^ ]*)", Opts),
    NameAtom = binary_to_atom(Name, utf8),
    case lists:member(NameAtom, IgnoreDeps) of
        true -> [];
        false -> [elvis_result:new(item, Message, [Name, Regex])]
    end;
erlang_mk_dep_to_result(Line, Message, IgnoreDeps) ->
    Opts = [{capture, all_but_first, binary}],
    {match, [Name]} = re:run(Line, "dep_([^ ]*)", Opts),
    NameAtom = binary_to_atom(Name, utf8),
    case lists:member(NameAtom, IgnoreDeps) of
        true -> [];
        false -> [elvis_result:new(item, Message, [Name])]
    end.

%% Old config

is_old_config(ElvisConfig) ->
    case proplists:get_value(config, ElvisConfig) of
        undefined -> false;
        Config when is_map(Config) -> true;
        Config when is_list(Config) ->
            SrcDirsIsKey = fun(RuleGroup) ->
                                   maps:is_key(src_dirs, RuleGroup) orelse
                                       exists_old_rule(RuleGroup)
                           end,
            lists:filter(SrcDirsIsKey, Config) /= []
    end.

exists_old_rule(#{rules := Rules}) ->
    Filter = fun
                 ({_, _, Args}) when is_list(Args) ->
                     true;
                 (_) ->
                     false
             end,
    lists:filter(Filter, Rules) /= [];
exists_old_rule(_) -> false.

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
