-module(elvis_file).

-export([
         src/1,
         path/1,
         parse_tree/2,
         parse_tree/3,
         load_file_data/2,

         find_files/2,
         filter_files/4,
         module/1
        ]).

-export_type([file/0]).

-type file() :: #{path => string(), content => binary(), _ => _}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns a tuple with the contents of the file and the file itself.
-spec src(file()) ->
    {binary(), file()} | {error, enoent}.
src(File = #{content := Content, encoding := _}) ->
    {Content, File};
src(File = #{content := Content}) ->
    {Content, File#{encoding => find_encoding(Content)}};
src(File = #{path := Path}) ->
    case file:read_file(Path) of
        {ok, Content} ->
            Encoding = find_encoding(Content),
            src(File#{content => Content,
                      encoding => Encoding});
        Error -> Error
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
-spec parse_tree(elvis_config:configs() | elvis_config:config(), file(), elvis_core:rule_config())
    -> {ktn_code:tree_node(), file()}.
parse_tree(_Config, File = #{parse_tree := ParseTree}, _RuleConfig) ->
    {ParseTree, File};
parse_tree(Config, File = #{path := Path, content := Content}, RuleConfig) ->
    Ext = filename:extension(Path),
    ExtStr = elvis_utils:to_str(Ext),
    Mod = module(File),
    Ignore = maps:get(ignore, RuleConfig, []),
    ParseTree = resolve_parse_tree(ExtStr, Content, Mod, Ignore),
    File1 = maybe_add_abstract_parse_tree(Config, File, Mod, Ignore),
    parse_tree(Config, File1#{parse_tree => ParseTree}, RuleConfig);
parse_tree(Config, File0 = #{path := _Path}, RuleConfig) ->
    {_, File} = src(File0),
    parse_tree(Config, File, RuleConfig);
parse_tree(_Config, File, _RuleConfig) ->
    throw({invalid_file, File}).

%% @doc Loads and adds all related file data.
-spec load_file_data(elvis_config:configs() | elvis_config:config(), file()) -> file().
load_file_data(Config, File0 = #{path := _Path}) ->
    {_, File1} = src(File0),
    {_, File2} = parse_tree(Config, File1),
    File2.

%% @doc Returns all files under the specified Path
%% that match the pattern Name.
-spec find_files([string()], string()) -> [file()].
find_files(Dirs, Pattern) ->
    Fun = fun(Dir) ->
              filelib:wildcard(filename:join(Dir, Pattern))
          end,
    [#{path => Path} || Path <- lists:usort(lists:flatmap(Fun, Dirs))].

%% @doc Filter files based on the glob provided.
-spec filter_files([file()], [string()], string(), [string()]) -> [file()].
filter_files(Files, Dirs, Filter, IgnoreList) ->
    AppendFilter = fun(Dir) ->
                         case Dir of
                             "." -> Filter;
                             Dir -> reduce_stars(Dir ++ "/" ++ Filter)
                         end
                   end,
    FullFilters = lists:map(AppendFilter, Dirs),
    Regexes = lists:map(fun glob_to_regex/1, FullFilters),
    FlatmapFun =
        fun(Regex) ->
                FilterFun =
                    fun(#{path := Path}) ->
                            match == re:run(Path, Regex, [{capture, none}])
                    end,
                lists:filter(FilterFun, Files)
        end,
    IgnoreFun =
        fun(#{path := Path}) ->
                IsIgnored =
                    fun(Regex) ->
                            match == re:run(Path, Regex, [{capture, none}])
                    end,
                not lists:any(IsIgnored, IgnoreList)
        end,
    Found = lists:flatmap(FlatmapFun, Regexes),
    % File src/sub/file.erl will match both src/ and src/sub/ folders. We can't have that!
    FoundUnique = lists:usort(Found),
    lists:filter(IgnoreFun, FoundUnique).

%% @doc Return module name corresponding to a given .erl/.beam file
-spec module(file()) -> module().
module(#{ path := Path }) ->
    list_to_atom(filename:basename(filename:basename(Path, ".erl"), ".beam")).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec resolve_parse_tree(string(), string() | binary(), module(), list()) ->
    undefined | ktn_code:tree_node().
resolve_parse_tree(".erl", Content, Mod, Ignore) ->
    Tree = ktn_code:parse_tree(Content),
    TreeContent = maps:get(content, Tree, []),
    FilteredTreeContent =
        lists:filter(fun (#{ type := function
                           , attrs := #{ name := FunName
                                       , arity := FunArity
                                       }
                           }) ->
                             not(lists:member({Mod, FunName}, Ignore)
                                 orelse lists:member({Mod, FunName, FunArity}, Ignore));
                         (_) -> true
        end,
        TreeContent),
    Tree#{ content => FilteredTreeContent };
resolve_parse_tree(_, _, _, _) ->
    undefined.

-spec glob_to_regex(iodata()) -> iodata().
glob_to_regex(Glob) ->
  add_delimiters(replace_questions(replace_stars(escape_all_chars(Glob)))).

add_delimiters(Glob) -> [$^, Glob, $$].

escape_all_chars(Glob) -> re:replace(Glob, ".", "[&]", [global]).

replace_stars(Glob) -> re:replace(Glob, "[[][*][]]", ".*", [global]).

replace_questions(Glob) -> re:replace(Glob, "[[][?][]]", ".", [global]).

reduce_stars(DirAndFilter) ->
    re:replace(DirAndFilter, "/\\*+/", "/", [global, {return, list}]).

-spec find_encoding(Content::binary()) ->
  atom().
find_encoding(Content) ->
    case epp:read_encoding_from_binary(Content) of
        none -> utf8;
        Enc  -> Enc
    end.

-spec maybe_add_abstract_parse_tree(Config, File, Mod, Ignore) -> Res when
      Config :: elvis_config:configs() | elvis_config:config(),
      File :: file(),
      Mod :: module(),
      Ignore :: [elvis_style:ignorable()],
      Res :: file().
maybe_add_abstract_parse_tree(#{ruleset := beam_files},
                              File = #{path := Path}, Mod, Ignore) ->
    AbstractParseTree = get_abstract_parse_tree(Path, Mod, Ignore),
    File#{abstract_parse_tree => AbstractParseTree};
maybe_add_abstract_parse_tree(_Config, File, _Mod, _Ignore) ->
    File.

-spec get_abstract_parse_tree(BeamPath, Mod, Ignore) -> Res when
      BeamPath :: file:filename(),
      Mod :: module(),
      Ignore :: [elvis_style:ignorable()],
      Res :: ktn_code:tree_node() | undefined.
get_abstract_parse_tree(BeamPath, Mod, Ignore) ->
    AbstractSrc = get_abstract_source(BeamPath),
    resolve_parse_tree(".erl", AbstractSrc, Mod, Ignore).

-spec get_abstract_source(BeamPath) -> Res when
    BeamPath :: file:filename() | binary(),
    Res :: string().
get_abstract_source(BeamPath) ->
    {ok, Src} = ktn_code:beam_to_string(BeamPath),
    Src.
