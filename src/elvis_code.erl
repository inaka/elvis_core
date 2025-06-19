-module(elvis_code).

%% General
-export([
    find/2,
    find/3,
    find_by_location/2,
    find_by_types/2,
    find_by_types/3,
    find_by_types/4,
    find_by_types_in_tokens/2,
    find_token/2,
    code_zipper/1,
    code_zipper/2
]).
%% Specific
-export([
    print_node/1,
    print_node/2
]).

-export_type([find_options/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type find_options() :: #{mode => node | zipper, traverse => content | all}.

%% @doc Same as calling find/3 with `#{mode => node, traverse => content}' as
%%      the options map.
%% @end
-spec find(fun((zipper:zipper(_)) -> boolean()), ktn_code:tree_node()) ->
    [ktn_code:tree_node()].
find(Pred, Root) ->
    find(Pred, Root, #{}).

%% @doc Find all nodes in the tree for which the predicate function returns
%%      `true'. The options map has two keys:
%%      <ul>
%%        <li>
%%        - `mode': when the value `node' is specified the predicate function
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
-spec find(fun((zipper:zipper(_)) -> boolean()), ktn_code:tree_node(), find_options()) ->
    [ktn_code:tree_node()].
find(Pred, Root, Opts) ->
    Mode = maps:get(mode, Opts, node),
    ZipperMode = maps:get(traverse, Opts, content),
    Zipper = code_zipper(Root, ZipperMode),
    Results = find(Pred, Zipper, [], Mode),
    lists:reverse(Results).

-spec code_zipper(ktn_code:tree_node()) -> zipper:zipper(_).
code_zipper(Root) ->
    code_zipper(Root, content).

-spec code_zipper(ktn_code:tree_node(), content | all) -> zipper:zipper(_).
code_zipper(Root, Mode) ->
    case Mode of
        content ->
            content_zipper(Root);
        all ->
            all_zipper(Root)
    end.

-spec content_zipper(ktn_code:tree_node()) -> zipper:zipper(_).
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

-spec all_zipper(ktn_code:tree_node()) -> zipper:zipper(_).
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

find(Pred, Zipper, Results, Mode) ->
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
                        [zipper:node(Zipper) | Results];
                    false ->
                        Results
                end,
            find(Pred, zipper:next(Zipper), NewResults, Mode)
    end.

-spec find_by_location(ktn_code:tree_node(), {integer(), integer()}) ->
    not_found | {ok, ktn_code:tree_node()}.
find_by_location(Root, Location) ->
    Fun = fun(Node) -> is_at_location(Node, Location) end,
    case find(Fun, Root, #{traverse => all}) of
        [] ->
            not_found;
        [Node | _] ->
            {ok, Node}
    end.

-spec find_by_types(Types, Root) -> Found when
    Types :: [ktn_code:tree_node_type()],
    Node :: ktn_code:tree_node(),
    Root :: Node,
    Found :: [Node].
find_by_types(Types, Root) ->
    find_by_types(Types, Root, _Pred = undefined).

-spec find_by_types(Types, Root, Filter) -> Found when
    Types :: [ktn_code:tree_node_type()],
    Node :: ktn_code:tree_node(),
    Root :: Node,
    Filter :: undefined | fun((Node) -> Node),
    Found :: [Node].
find_by_types(Types, Root, Filter) ->
    find_by_types(Types, Root, Filter, #{}).

-spec find_by_types(Types, Root, Filter, Opts) -> Found when
    Types :: [ktn_code:tree_node_type()],
    Node :: ktn_code:tree_node(),
    Root :: Node,
    Filter :: undefined | fun((Node) -> Node),
    Opts :: find_options(),
    Found :: [Node].
find_by_types(Types, Root, Filter, Opts) ->
    NonFilteredResults = find(
        fun(Node) ->
            lists:member(ktn_code:type(Node), Types)
        end,
        Root,
        Opts
    ),
    case Filter of
        undefined ->
            NonFilteredResults;
        _ ->
            [Result || Result <- NonFilteredResults, Filter(Result)]
    end.

find_by_types_in_tokens(Types, Root) ->
    Tokens = ktn_code:attr(tokens, Root),
    lists:filter(
        fun(Node) ->
            lists:member(ktn_code:type(Node), Types)
        end,
        Tokens
    ).

is_at_location(#{attrs := #{location := {Line, NodeCol}}} = Node, {Line, Column}) ->
    Text = ktn_code:attr(text, Node),
    Length = length(Text),
    NodeCol =< Column andalso Column < NodeCol + Length;
is_at_location(_, _) ->
    false.

-spec find_token(ktn_code:tree_node(), {integer(), integer()}) -> not_found | {ok, map()}.
find_token(Root, Location) ->
    Fun = fun(Token) -> is_at_location(Token, Location) end,
    Tokens = ktn_code:attr(tokens, Root),
    case lists:filter(Fun, Tokens) of
        [] ->
            not_found;
        [Token | _] ->
            {ok, Token}
    end.

%% @doc Debugging utility function.
-spec print_node(ktn_code:tree_node()) -> ok.
print_node(Node) ->
    print_node(Node, 0).

%% @doc Debugging utility function.
-spec print_node(ktn_code:tree_node(), integer()) -> ok.
print_node(#{type := Type} = Node, CurrentLevel) ->
    Type = ktn_code:type(Node),
    Indentation = lists:duplicate(CurrentLevel * 4, $\s),
    Content = ktn_code:content(Node),

    ok = elvis_utils:info("~s - [~p] ~p~n", [Indentation, CurrentLevel, Type]),
    _ = lists:map(fun(Child) -> print_node(Child, CurrentLevel + 1) end, Content),
    ok.
