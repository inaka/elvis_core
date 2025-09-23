-module(elvis_file).

-export([
    src/1,
    path/1,
    parse_tree/2,
    load_file_data/2,
    find_files/2,
    filter_files/4,
    module/1
]).

-export_type([t/0]).

-type t() ::
    #{
        path => string(),
        content => binary(),
        _ => _
    }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns a tuple with the contents of the file and the file itself.
-spec src(t()) -> {binary(), t()} | {error, enoent}.
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

%% @doc Given a t() returns its path.
-spec path(t()) -> string().
path(#{path := Path}) ->
    Path;
path(File) ->
    throw({invalid_file, File}).

%% @doc Add the root node of the parse tree to the file data, with filtering.
-spec parse_tree(
    elvis_rule:t() | elvis_file:t(),
    [elvis_config:t()] | elvis_config:t()
) ->
    {ktn_code:tree_node(), t()}.
parse_tree(File, ElvisConfig) when is_map(File) ->
    Rule = elvis_rule:new(no_namespace, no_rule, elvis_rule:defmap(#{})),
    parse_tree(elvis_rule:file(Rule, File), ElvisConfig);
parse_tree(Rule, ElvisConfig) ->
    case elvis_rule:file(Rule) of
        #{parse_tree := ParseTree0} = File ->
            Module = module(File),
            {filter_tree_for(ParseTree0, Module, Rule), File};
        #{path := Path, content := Content} = File0 ->
            Ext = filename:extension(Path),
            ExtStr = elvis_utils:to_str(Ext),
            Module = module(File0),
            ParseTree = resolve_parse_tree(ExtStr, Content, Module, Rule),
            File = maybe_add_abstract_parse_tree(ElvisConfig, File0, Module, Rule),
            parse_tree(elvis_rule:file(Rule, File#{parse_tree => ParseTree}), ElvisConfig);
        #{path := _Path} = File0 ->
            {_, File} = src(File0),
            parse_tree(elvis_rule:file(Rule, File), ElvisConfig);
        File ->
            throw({invalid_file, File})
    end.

%% @doc Loads and adds all related file data.
-spec load_file_data([elvis_config:t()] | elvis_config:t(), t()) -> t().
load_file_data(ElvisConfig, #{path := _Path} = File0) ->
    {_, File1} = src(File0),
    {_, File2} = parse_tree(File1, ElvisConfig),
    File2.

%% @doc Returns all files under the specified Path
%% that match the pattern Name.
-spec find_files([string()], string()) -> [t()].
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
-spec filter_files([t()], [string()], string(), [string()]) -> [t()].
filter_files(Files, Dirs, Filter, IgnoreList) ->
    ExpandedFilters = lists:map(fun(Dir) -> filelib:wildcard(dir_to(Filter, Dir)) end, Dirs),
    Found =
        lists:flatmap(fun(ExpandedFilter) -> file_in(ExpandedFilter, Files) end, ExpandedFilters),
    % File src/sub/file.erl will match both src/ and src/sub/ folders. We can't have that!
    FoundUnique = lists:usort(Found),

    lists:filter(
        fun(#{path := Path}) ->
            MatchesPath = fun(Regex) -> match =:= re:run(Path, Regex, [{capture, none}]) end,
            not lists:any(MatchesPath, IgnoreList)
        end,
        FoundUnique
    ).

%% @doc Return module name corresponding to a given .hrl/.erl/.beam file
-spec module(t()) -> module().
module(#{path := Path}) ->
    BaseName = filename:basename(Path),
    Stripped = lists:foldl(
        fun(Ext, Acc) ->
            filename:basename(Acc, Ext)
        end,
        BaseName,
        [".hrl", ".erl", ".beam"]
    ),
    list_to_atom(Stripped).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec resolve_parse_tree(string(), string() | binary(), module(), elvis_rule:t()) ->
    undefined | ktn_code:tree_node().
resolve_parse_tree(Ext, Content, Module, Rule) when Ext =:= ".erl"; Ext =:= ".hrl" ->
    Tree = ktn_code:parse_tree(Content),
    filter_tree_for(Tree, Module, Rule);
resolve_parse_tree(_, _, _, _) ->
    undefined.

filter_tree_for(Tree, Module, Rule) when is_map(Tree) ->
    TreeContent = maps:get(content, Tree, []),
    Tree#{
        content =>
            lists:filter(
                fun
                    (
                        #{
                            type := function,
                            attrs := #{name := Function, arity := Arity}
                        }
                    ) ->
                        MF = elvis_rule:ignorable({Module, Function}),
                        MFA = elvis_rule:ignorable({Module, Function, Arity}),
                        not elvis_rule:ignored(MF, Rule) andalso
                            not elvis_rule:ignored(MFA, Rule);
                    (_) ->
                        true
                end,
                TreeContent
            )
    };
filter_tree_for(Tree, _Mod, _Rule) ->
    Tree.

-spec find_encoding(Content :: binary()) -> atom().
find_encoding(Content) ->
    case epp:read_encoding_from_binary(Content) of
        none ->
            utf8;
        Enc ->
            Enc
    end.

-spec maybe_add_abstract_parse_tree(ElvisConfig, File, Module, Rule) -> Res when
    ElvisConfig :: [elvis_config:t()] | elvis_config:t(),
    File :: t(),
    Module :: module(),
    Rule :: elvis_rule:t(),
    Res :: t().
maybe_add_abstract_parse_tree(
    #{ruleset := Ruleset},
    #{path := Path} = File,
    Module,
    Rule
) when Ruleset =:= beam_files; Ruleset =:= beam_files_strict ->
    AbstractParseTree = get_abstract_parse_tree(Path, Module, Rule),
    File#{abstract_parse_tree => AbstractParseTree};
maybe_add_abstract_parse_tree(_ElvisConfig, File, _Mod, _Rule) ->
    File.

-spec get_abstract_parse_tree(BeamPath, Module, Rule) -> Res when
    BeamPath :: file:filename(),
    Module :: module(),
    Rule :: elvis_rule:t(),
    Res :: ktn_code:tree_node() | undefined.
get_abstract_parse_tree(BeamPath, Module, Rule) ->
    AbstractSrc = get_abstract_source(BeamPath),
    resolve_parse_tree(".erl", AbstractSrc, Module, Rule).

-spec get_abstract_source(BeamPath) -> Res when
    BeamPath :: file:filename() | binary(),
    Res :: string().
get_abstract_source(BeamPath) ->
    {ok, Src} = ktn_code:beam_to_string(BeamPath),
    Src.
