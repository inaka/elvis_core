-module(elvis_code).

%% General
-export([
    find/1,
    zipper/1,
    root/2
]).
%% Specific
-export([
    print_node/1,
    print_node/2
]).

% These are local debug functions.
-ignore_xref([print_node/1, print_node/2]).
% Kept for backward compatibility; no longer used internally.
-ignore_xref([zipper/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type tree_node() :: ktn_code:tree_node().
-type tree_node_zipper() :: zipper:zipper(tree_node()).

-export_type([tree_node/0, tree_node_zipper/0]).

-spec find(Options) -> {nodes, [Node]} | {zippers, [Zipper]} when
    Options :: #{
        % undefined means "all types"
        of_types := [ktn_code:tree_node_type()] | undefined,
        inside := Node,
        % undefined means "don't filter"
        filtered_by => fun((Node | Zipper) -> boolean()),
        filtered_from => node | zipper,
        traverse => content | all
    },
    Node :: tree_node(),
    Zipper :: tree_node_zipper().
find(#{of_types := OfTypes, inside := Inside} = Options) ->
    FilteredBy = maps:get(filtered_by, Options, undefined),
    FilteredFrom = maps:get(filtered_from, Options, node),
    Traverse = maps:get(traverse, Options, content),

    case FilteredFrom of
        node ->
            Pred = build_node_pred(OfTypes, FilteredBy),
            {nodes, find_nodes(Pred, Inside, Traverse)};
        node_and_ancestors ->
            Pred = build_ancestors_pred(OfTypes, FilteredBy),
            {nodes_and_ancestors, find_nodes_with_ancestors(Pred, Inside, Traverse)};
        zipper ->
            TypePred =
                fun(Zipper) ->
                    OfTypes =:= undefined orelse
                        lists:member(ktn_code:type(zipper:node(Zipper)), OfTypes)
                end,
            ZipperObj = zipper(Inside, Traverse),
            NonFilteredResults = find_with_zipper(TypePred, ZipperObj, [], #{}, zipper),
            Results =
                case FilteredBy of
                    undefined -> NonFilteredResults;
                    _ -> [Z || Z <- NonFilteredResults, FilteredBy(Z)]
                end,
            {zippers, Results}
    end.

build_node_pred(OfTypes, undefined) ->
    fun(Node) ->
        OfTypes =:= undefined orelse lists:member(ktn_code:type(Node), OfTypes)
    end;
build_node_pred(OfTypes, FilteredBy) ->
    fun(Node) ->
        (OfTypes =:= undefined orelse lists:member(ktn_code:type(Node), OfTypes)) andalso
            FilteredBy(Node)
    end.

build_ancestors_pred(OfTypes, undefined) ->
    fun({Node, _Ancestors}) ->
        OfTypes =:= undefined orelse lists:member(ktn_code:type(Node), OfTypes)
    end;
build_ancestors_pred(OfTypes, FilteredBy) ->
    fun({Node, _Ancestors} = NodeAndAncestors) ->
        (OfTypes =:= undefined orelse lists:member(ktn_code:type(Node), OfTypes)) andalso
            FilteredBy(NodeAndAncestors)
    end.

find_nodes_with_ancestors(Pred, Node, content) ->
    find_nodes_content_ancestors(Pred, Node, []);
find_nodes_with_ancestors(Pred, Node, all) ->
    {Results, _Seen} = find_nodes_all_ancestors(Pred, Node, [], #{}),
    Results.

find_nodes_content_ancestors(Pred, #{content := Content} = Node, Ancestors) ->
    Match = [{Node, Ancestors} || Pred({Node, Ancestors})],
    Match ++
        lists:flatmap(
            fun(Child) -> find_nodes_content_ancestors(Pred, Child, [Node | Ancestors]) end,
            Content
        );
find_nodes_content_ancestors(Pred, Node, Ancestors) when is_map(Node) ->
    [{Node, Ancestors} || Pred({Node, Ancestors})];
find_nodes_content_ancestors(_Pred, _Node, _Ancestors) ->
    [].

find_nodes_all_ancestors(Pred, Node, Ancestors, Seen) when is_map(Node) ->
    case Seen of
        #{Node := _} ->
            {[], Seen};
        #{} ->
            Seen1 = Seen#{Node => true},
            Match = [{Node, Ancestors} || Pred({Node, Ancestors})],
            NewAncestors = [Node | Ancestors],
            {ChildResults, Seen2} =
                lists:foldl(
                    fun(Child, {A, S}) ->
                        {R, S1} = find_nodes_all_ancestors(Pred, Child, NewAncestors, S),
                        {[R | A], S1}
                    end,
                    {[], Seen1},
                    all_children(Node)
                ),
            {Match ++ lists:append(lists:reverse(ChildResults)), Seen2}
    end;
find_nodes_all_ancestors(_Pred, _Node, _Ancestors, Seen) ->
    {[], Seen}.

find_nodes(Pred, Node, content) ->
    find_nodes_content(Pred, Node);
find_nodes(Pred, Node, all) ->
    {Results, _Seen} = find_nodes_all(Pred, Node, #{}),
    Results.

find_nodes_content(Pred, #{content := Content} = Node) ->
    Match = [Node || Pred(Node)],
    Match ++ lists:flatmap(fun(Child) -> find_nodes_content(Pred, Child) end, Content);
find_nodes_content(Pred, Node) when is_map(Node) ->
    [Node || Pred(Node)];
find_nodes_content(_Pred, _Node) ->
    [].

find_nodes_all(Pred, Node, Seen) when is_map(Node) ->
    case Seen of
        #{Node := _} ->
            {[], Seen};
        #{} ->
            Seen1 = Seen#{Node => true},
            Match = [Node || Pred(Node)],
            {ChildResults, Seen2} =
                lists:foldl(
                    fun(Child, {A, S}) ->
                        {R, S1} = find_nodes_all(Pred, Child, S),
                        {[R | A], S1}
                    end,
                    {[], Seen1},
                    all_children(Node)
                ),
            {Match ++ lists:append(lists:reverse(ChildResults)), Seen2}
    end;
find_nodes_all(_Pred, _Node, Seen) ->
    {[], Seen}.

all_children(#{content := Content, node_attrs := NodeAttrs}) ->
    Content ++ lists:flatten(maps:values(NodeAttrs));
all_children(#{content := Content}) ->
    Content;
all_children(#{node_attrs := NodeAttrs}) ->
    lists:flatten(maps:values(NodeAttrs));
all_children(_) ->
    [].

-spec zipper(tree_node()) -> tree_node_zipper().
zipper(Root) ->
    zipper(Root, content).

-spec zipper(tree_node(), content | all) -> tree_node_zipper().
zipper(Root, content) ->
    content_zipper(Root);
zipper(Root, all) ->
    all_zipper(Root).

-spec content_zipper(tree_node()) -> tree_node_zipper().
content_zipper(Root) ->
    IsBranch =
        fun
            (#{content := [_ | _]}) ->
                true;
            (_) ->
                false
        end,
    Children = fun(#{content := Content}) -> Content end,
    MakeNode = fun(Node, Content) -> Node#{content => Content} end,
    zipper:new(IsBranch, Children, MakeNode, Root).

-spec all_zipper(tree_node()) -> tree_node_zipper().
all_zipper(Root) ->
    IsBranch =
        fun(#{} = Node) -> ktn_code:content(Node) =/= [] orelse maps:is_key(node_attrs, Node) end,
    Children =
        fun
            (#{content := Content, node_attrs := NodeAttrs}) ->
                Content ++
                    lists:flatten(
                        maps:values(NodeAttrs)
                    );
            (#{node_attrs := NodeAttrs}) ->
                lists:flatten(
                    maps:values(NodeAttrs)
                );
            (#{content := Content}) ->
                Content
        end,
    MakeNode = fun(Node, _) -> Node end,
    zipper:new(IsBranch, Children, MakeNode, Root).

find_with_zipper(Pred, Zipper, Results, Keys, Mode) ->
    case zipper:is_end(Zipper) of
        true ->
            lists:reverse(Results);
        false ->
            Value =
                case Mode of
                    zipper ->
                        Zipper;
                    node ->
                        zipper:node(Zipper)
                end,
            {NewResults, NewKeys} =
                case Pred(Value) of
                    true ->
                        case is_map_key(Value, Keys) of
                            true ->
                                %% Note: I'm not sure why, sometimes, traversing a zipper may
                                %%       result in going through the same node twice, but it has
                                %%       happened. You can see it for yourself: Just add a ct:pal
                                %%       here, run the tests and simplify_anonymous_functions will
                                %%       show duplicate results.
                                {Results, Keys};
                            false ->
                                {[Value | Results], Keys#{Value => true}}
                        end;
                    false ->
                        {Results, Keys}
                end,
            find_with_zipper(Pred, zipper:next(Zipper), NewResults, NewKeys, Mode)
    end.

-spec root(Rule, ElvisConfig) -> Res when
    Rule :: elvis_rule:t(),
    ElvisConfig :: elvis_config:t(),
    Res :: ktn_code:tree_node().
root(Rule, ElvisConfig) ->
    {Root0, File0} = elvis_file:parse_tree(Rule, ElvisConfig),
    case elvis_config:ruleset(ElvisConfig) of
        Ruleset when Ruleset =:= beam_files; Ruleset =:= beam_files_strict ->
            elvis_file:get_abstract_parse_tree(File0);
        _ ->
            Root0
    end.

%% @doc Debugging utility function.
-spec print_node(tree_node()) -> ok.
print_node(Node) ->
    print_node(Node, 0).

%% @doc Debugging utility function.
-spec print_node(tree_node(), integer()) -> ok.
print_node(#{type := Type} = Node, CurrentLevel) ->
    Type = ktn_code:type(Node),
    Indentation = lists:duplicate(CurrentLevel * 4, $\s),
    Content = ktn_code:content(Node),

    _ = elvis_utils:info("~s - [~p] ~p", [Indentation, CurrentLevel, Type]),
    _ = lists:map(fun(Child) -> print_node(Child, CurrentLevel + 1) end, Content),
    ok.
