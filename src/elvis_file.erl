-module(elvis_file).

-export([
    src/1,
    path/1,
    parse_tree/2, parse_tree/3,
    load_file_data/2,
    find_files/2,
    filter_files/4,
    module/1
]).

-export_type([file/0]).

-type file() ::
    #{
        path => string(),
        content => binary(),
        _ => _
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns a tuple with the contents of the file and the file itself.
-spec src(file()) -> {binary(), file()} | {error, enoent}.
src(#{content := Content, encoding := _} = File) ->
    {Content, File};
src(#{content := Content} = File) ->
    {Content, File#{encoding => find_encoding(Content)}};
src(#{path := Path} = File) ->
    case file:read_file(Path) of
        {ok, Content} ->
            Encoding = find_encoding(Content),
            src(File#{content => Content, encoding => Encoding});
        Error ->
            Error
    end;
src(File) ->
    throw({invalid_file, File}).

%% @doc Given a file() returns its path.
-spec path(file()) -> string().
path(#{path := Path}) ->
    Path;
path(File) ->
    throw({invalid_file, File}).

%% @doc Add the root node of the parse tree to the file data.
-spec parse_tree(elvis_config:configs() | elvis_config:config(), file()) ->
    {ktn_code:tree_node(), file()}.
parse_tree(Config, Target) ->
    parse_tree(Config, Target, _RuleConfig = #{}).

%% @doc Add the root node of the parse tree to the file data, with filtering.
-spec parse_tree(
    elvis_config:configs() | elvis_config:config(),
    file(),
    elvis_core:rule_config()
) ->
    {ktn_code:tree_node(), file()}.
parse_tree(_Config, #{parse_tree := ParseTree0} = File, RuleConfig) ->
    Ignore = maps:get(ignore, RuleConfig, []),
    Mod = module(File),
    {filter_tree_for(ParseTree0, Mod, Ignore), File};
parse_tree(Config, #{path := Path, content := Content} = File, RuleConfig) ->
    Ext = filename:extension(Path),
    ExtStr = elvis_utils:to_str(Ext),
    Mod = module(File),
    Ignore = maps:get(ignore, RuleConfig, []),
    ParseTree = resolve_parse_tree(ExtStr, Content, Mod, Ignore),
    File1 = maybe_add_abstract_parse_tree(Config, File, Mod, Ignore),
    parse_tree(Config, File1#{parse_tree => ParseTree}, RuleConfig);
parse_tree(Config, #{path := _Path} = File0, RuleConfig) ->
    {_, File} = src(File0),
    parse_tree(Config, File, RuleConfig);
parse_tree(_Config, File, _RuleConfig) ->
    throw({invalid_file, File}).

%% @doc Loads and adds all related file data.
-spec load_file_data(elvis_config:configs() | elvis_config:config(), file()) -> file().
load_file_data(Config, #{path := _Path} = File0) ->
    {_, File1} = src(File0),
    {_, File2} = parse_tree(Config, File1),
    File2.

%% @doc Returns all files under the specified Path
%% that match the pattern Name.
-spec find_files([string()], string()) -> [file()].
find_files(Dirs, Pattern) ->
    Fun = fun(Dir) ->
        filelib:wildcard(
            filename:join(Dir, Pattern)
        )
    end,
    [
        #{path => Path}
     || Path <-
            lists:usort(
                lists:flatmap(Fun, Dirs)
            )
    ].

dir_to(Filter, ".") ->
    Filter;
dir_to(Filter, Dir) ->
    filename:join(Dir, Filter).

file_in(ExpandedFilter, Files) ->
    lists:filter(fun(#{path := Path}) -> lists:member(Path, ExpandedFilter) end, Files).

%% @doc Filter files based on the glob provided.
-spec filter_files([file()], [string()], string(), [string()]) -> [file()].
filter_files(Files, Dirs, Filter, IgnoreList) ->
    ExpandedFilters = lists:map(fun(Dir) -> filelib:wildcard(dir_to(Filter, Dir)) end, Dirs),
    Found =
        lists:flatmap(fun(ExpandedFilter) -> file_in(ExpandedFilter, Files) end, ExpandedFilters),
    % File src/sub/file.erl will match both src/ and src/sub/ folders. We can't have that!
    FoundUnique = lists:usort(Found),

    lists:filter(
        fun(#{path := Path}) ->
            MatchesPath = fun(Regex) -> match == re:run(Path, Regex, [{capture, none}]) end,
            not lists:any(MatchesPath, IgnoreList)
        end,
        FoundUnique
    ).

%% @doc Return module name corresponding to a given .hrl/.erl/.beam file
-spec module(file()) -> module().
module(#{path := Path}) ->
    list_to_atom(
        filename:basename(
            filename:basename(Path, ".erl"), ".beam"
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
-spec resolve_parse_tree(string(), string() | binary(), module(), list()) ->
    undefined | ktn_code:tree_node().
resolve_parse_tree(".erl", Content, Mod, Ignore) ->
    Tree = ktn_code:parse_tree(Content),
    filter_tree_for(Tree, Mod, Ignore);
resolve_parse_tree(".hrl", Content, Mod, Ignore) ->
    Tree = ktn_code:parse_tree(Content),
    filter_tree_for(Tree, Mod, Ignore);
resolve_parse_tree(_, _, _, _) ->
    undefined.

%% @private
filter_tree_for(Tree, Mod, Ignore) when is_map(Tree) ->
    TreeContent = maps:get(content, Tree, []),
    Tree#{
        content =>
            lists:filter(
                fun
                    (
                        #{
                            type := function,
                            attrs := #{name := FunName, arity := FunArity}
                        }
                    ) ->
                        not lists:member({Mod, FunName}, Ignore) andalso
                            not lists:member({Mod, FunName, FunArity}, Ignore);
                    (_) ->
                        true
                end,
                TreeContent
            )
    };
filter_tree_for(Tree, _Mod, _Ignore) ->
    Tree.

%% @private
-spec find_encoding(Content :: binary()) -> atom().
find_encoding(Content) ->
    case epp:read_encoding_from_binary(Content) of
        none ->
            utf8;
        Enc ->
            Enc
    end.

%% @private
-spec maybe_add_abstract_parse_tree(Config, File, Mod, Ignore) -> Res when
    Config :: elvis_config:configs() | elvis_config:config(),
    File :: file(),
    Mod :: module(),
    Ignore :: [elvis_style:ignorable()],
    Res :: file().
maybe_add_abstract_parse_tree(
    #{ruleset := beam_files},
    #{path := Path} = File,
    Mod,
    Ignore
) ->
    AbstractParseTree = get_abstract_parse_tree(Path, Mod, Ignore),
    File#{abstract_parse_tree => AbstractParseTree};
maybe_add_abstract_parse_tree(_Config, File, _Mod, _Ignore) ->
    File.

%% @private
-spec get_abstract_parse_tree(BeamPath, Mod, Ignore) -> Res when
    BeamPath :: file:filename(),
    Mod :: module(),
    Ignore :: [elvis_style:ignorable()],
    Res :: ktn_code:tree_node() | undefined.
get_abstract_parse_tree(BeamPath, Mod, Ignore) ->
    AbstractSrc = get_abstract_source(BeamPath),
    resolve_parse_tree(".erl", AbstractSrc, Mod, Ignore).

%% @private
-spec get_abstract_source(BeamPath) -> Res when
    BeamPath :: file:filename() | binary(),
    Res :: string().
get_abstract_source(BeamPath) ->
    {ok, Src} = ktn_code:beam_to_string(BeamPath),
    Src.
