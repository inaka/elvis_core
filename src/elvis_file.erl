-module(elvis_file).

-export([
         src/1,
         path/1,
         parse_tree/2,
         load_file_data/2,

         find_files/2,
         filter_files/4
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
-spec parse_tree(elvis_config:config() | map(), file()) ->
  {ktn_code:tree_node(), file()}.
parse_tree(_Config, File = #{parse_tree := ParseTree}) ->
    {ParseTree, File};
parse_tree(Config, File = #{path := Path, content := Content}) ->
    Ext = filename:extension(Path),
    ExtStr = elvis_utils:to_str(Ext),
    ParseTree = resolve_parse_tree(ExtStr, Content),
    parse_tree(Config, File#{parse_tree => ParseTree});
parse_tree(Config, File0 = #{path := _Path}) ->
    {_, File} = src(File0),
    parse_tree(Config, File);
parse_tree(_Config, File) ->
    throw({invalid_file, File}).

%% @doc Loads and adds all related file data.
-spec load_file_data(map(), file()) -> file().
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec resolve_parse_tree(string(), binary()) ->
    undefined | ktn_code:tree_node().
resolve_parse_tree(".erl", Content) ->
    ktn_code:parse_tree(Content);
resolve_parse_tree( _, _) ->
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
