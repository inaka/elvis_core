-module(elvis_code).

%% General
-export([find/2, find/3, find_by_location/2, find_token/2, code_zipper/1, code_zipper/2]).
%% Specific
-export([
    past_nesting_limit/2,
    exported_functions/1,
    exported_types/1,
    function_names/1,
    module_name/1,
    print_node/1, print_node/2
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
    find(Pred, Root, #{mode => node, traverse => content}).

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

%%% Processing functions

%% @doc Takes a node and returns all nodes where the nesting limit is exceeded.
-spec past_nesting_limit(ktn_code:tree_node(), integer()) ->
    [{ktn_code:tree_node(), integer()}].
past_nesting_limit(Node, MaxLevel) ->
    ResultNodes = past_nesting_limit(Node, 1, MaxLevel),
    lists:reverse(ResultNodes).

past_nesting_limit(Node, CurrentLevel, MaxLevel) when CurrentLevel > MaxLevel ->
    [Node];
past_nesting_limit(#{content := Content}, CurrentLevel, MaxLevel) ->
    Fun = fun(ChildNode) ->
        Increment = level_increment(ChildNode),
        past_nesting_limit(ChildNode, Increment + CurrentLevel, MaxLevel)
    end,
    lists:flatmap(Fun, Content);
past_nesting_limit(_Node, _CurrentLeve, _MaxLevel) ->
    [].

%% @doc Debugging utility function.
-spec print_node(ktn_code:tree_node()) -> ok.
print_node(Node) ->
    print_node(Node, 0).

-spec print_node(ktn_code:tree_node(), integer()) -> ok.
print_node(#{type := Type} = Node, CurrentLevel) ->
    Type = ktn_code:type(Node),
    Indentation = lists:duplicate(CurrentLevel * 4, $\s),
    Content = ktn_code:content(Node),

    ok = elvis_utils:info("~s - [~p] ~p~n", [Indentation, CurrentLevel, Type]),
    _ = lists:map(fun(Child) -> print_node(Child, CurrentLevel + 1) end, Content),
    ok.

%% @doc Takes the root node and returns the module's name.
-spec module_name(ktn_code:tree_node()) -> atom().
module_name(#{type := root, content := Content}) ->
    Fun = fun(#{type := Type}) -> Type == module end,
    case lists:filter(Fun, Content) of
        [ModuleNode | _] ->
            elvis_ktn:value(ModuleNode);
        [] ->
            undefined
    end.

%% @doc Takes the root node of a parse_tree and returns name and arity
%%      of each exported function.
-spec exported_functions(ktn_code:tree_node()) -> [{atom(), integer()}].
exported_functions(#{type := root, content := Content}) ->
    Fun = make_extractor_fun(exported_functions),
    lists:flatmap(Fun, Content).

-spec exported_types(ktn_code:tree_node()) -> [{atom(), integer()}].
exported_types(#{type := root, content := Content}) ->
    Fun = make_extractor_fun(exported_types),
    lists:flatmap(Fun, Content).

%% @doc Takes the root node of a parse_tree and returns the name
%%      of each function, whether exported or not.
-spec function_names(ktn_code:tree_node()) -> [atom()].
function_names(#{type := root, content := Content}) ->
    Fun = make_extractor_fun(function_names),
    lists:flatmap(Fun, Content).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
%% @doc Takes a node and determines its nesting level increment.
level_increment(#{type := 'fun', content := _}) ->
    1;
level_increment(#{type := 'fun'}) ->
    0;
level_increment(#{type := Type}) ->
    IncrementOne = [function, 'case', 'if', try_case, try_catch, named_fun, receive_case],
    case lists:member(Type, IncrementOne) of
        true ->
            1;
        false ->
            0
    end.

%% @private
%% @doc Returns an anonymous Fun to be flatmapped over node content, as
%% appropriate for the exported function whose name is the argument given.
make_extractor_fun(exported_functions) ->
    fun
        (#{type := export} = Node) ->
            elvis_ktn:value(Node);
        (_) ->
            []
    end;
make_extractor_fun(exported_types) ->
    fun
        (#{type := export_type} = Node) ->
            elvis_ktn:value(Node);
        (_) ->
            []
    end;
make_extractor_fun(function_names) ->
    fun
        (#{type := function} = Node) ->
            [ktn_code:attr(name, Node)];
        (_) ->
            []
    end.
