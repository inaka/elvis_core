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

    NonFilteredResults = find(
        fun(NodeOrZipper) ->
            Node =
                case FilteredFrom of
                    _ when OfTypes =:= undefined ->
                        undefined;
                    node ->
                        NodeOrZipper;
                    zipper ->
                        Zipper = NodeOrZipper,
                        zipper:node(Zipper)
                end,
            OfTypes =:= undefined orelse lists:member(ktn_code:type(Node), OfTypes)
        end,
        Inside,
        FilteredFrom,
        Traverse
    ),

    Results =
        case FilteredBy of
            undefined ->
                NonFilteredResults;
            _ ->
                [Result || Result <- NonFilteredResults, FilteredBy(Result)]
        end,

    case FilteredFrom of
        node ->
            {nodes, Results};
        zipper ->
            {zippers, Results}
    end.

%% @doc Find all nodes in the tree for which the predicate function returns
%%      `true'. The options map has two keys:
%%      <ul>
%%        <li>
%%        - `filtered_from': when the value `node' is specified the predicate function
%%          receives a tree_node() as its argument. When `zipper' is specified
%%          the argument is the zipper location for the current node.
%%        </li>
%%        <li>
%%        - `traverse': the value `content' indicates to only take into account
%%          nodes in the parent-child hierarchy. When `all' is provided the
%%          nodes held in the `node_attrs' map are also taken into account in
%%          the search.
%%        </li>
%%      </ul>
%% @end
-spec find(fun((tree_node_zipper()) -> boolean()), tree_node(), node | zipper, content | all) ->
    [tree_node() | tree_node_zipper()].
find(Pred, Root, FilteredFrom, Traverse) ->
    Zipper = zipper(Root, Traverse),
    Results = find_with_zipper(Pred, Zipper, [], FilteredFrom),
    lists:reverse(Results).

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

find_with_zipper(Pred, Zipper, Results, Mode) ->
    case zipper:is_end(Zipper) of
        true ->
            Results;
        false ->
            Value =
                case Mode of
                    zipper ->
                        Zipper;
                    node ->
                        zipper:node(Zipper)
                end,
            NewResults =
                case Pred(Value) of
                    true ->
                        [Value | Results];
                    false ->
                        Results
                end,
            find_with_zipper(Pred, zipper:next(Zipper), NewResults, Mode)
    end.

-spec root(Rule, ElvisConfig) -> Res when
    Rule :: elvis_rule:t(),
    ElvisConfig :: elvis_config:t(),
    Res :: ktn_code:tree_node().
root(Rule, ElvisConfig) ->
    {Root0, File0} = elvis_file:parse_tree(Rule, ElvisConfig),
    case maps:get(ruleset, ElvisConfig, undefined) of
        Ruleset when Ruleset =:= beam_files; Ruleset =:= beam_files_strict ->
            maps:get(abstract_parse_tree, File0);
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

    ok = elvis_utils:info("~s - [~p] ~p~n", [Indentation, CurrentLevel, Type]),
    _ = lists:map(fun(Child) -> print_node(Child, CurrentLevel + 1) end, Content),
    ok.
