-module(elvis_rule).

-export([
    new/2, new/3,
    from_tuple/1,
    is_valid_from_tuple/1,
    is_ignorable/1,
    ns/1,
    name/1,
    def/1,
    disabled/1,
    file/1, file/2,
    ignored/2,
    execute/2,
    option/2,
    defmap/1,
    defkeys/1,
    ignorable/1,
    same/2
]).

-record(rule, {
    ns :: module(),
    name :: atom(),
    rule_fun :: fun(),
    def = #{} :: def(),
    ignores = [] :: [ignorable()],
    disabled = false :: boolean(),
    file = undefined :: undefined | elvis_file:t()
}).
-opaque t() :: #rule{}.

-opaque def() :: #{
    atom() => term()
}.
-opaque ignorable() :: module() | {module(), atom()} | {module(), atom(), arity()}.

-export_type([t/0, def/0, ignorable/0]).

-callback default(RuleName :: atom()) -> def().

-spec new(NS :: module(), Name :: atom()) -> t().
new(NS, Name) ->
    new(NS, Name, default(NS, Name)).

-spec new(NS :: module(), Name :: atom(), Def :: def()) -> t().
new(NS, Name, Def) ->
    #rule{
        ns = NS,
        name = Name,
        rule_fun = fun NS:Name/2,
        def = Def,
        ignores = maps:get(ignore, Def, [])
    }.

-spec from_tuple(Rule | NSName | NSNameDef) -> t() | invalid_tuple when
    Rule :: t(),
    NSName :: {NS :: module(), Name :: atom()},
    NSNameDef :: {NS :: module(), Name :: atom(), Def :: disable | map()}.
from_tuple(Rule) when is_record(Rule, rule) ->
    Rule;
from_tuple({NS, Name}) ->
    from_tuple({NS, Name, #{}});
from_tuple({NS, Name, Def0}) when is_map(Def0) orelse Def0 =:= disable ->
    {Def, Disable} =
        case Def0 of
            disable ->
                {#{}, true};
            _ ->
                {Def0, false}
        end,
    Rule = new(NS, Name, Def),
    case Disable of
        true ->
            disable(Rule);
        false ->
            Rule
    end;
from_tuple(_) ->
    invalid_tuple.

-spec is_valid_from_tuple(tuple()) -> {true, t()} | {false, string()} | {deprecated, string()}.
is_valid_from_tuple({NS, Name}) when is_atom(NS), is_atom(Name) ->
    is_valid_from_tuple_check(NS, Name, #{});
is_valid_from_tuple({NS, Name, Def}) when is_atom(NS), is_atom(Name), is_map(Def) ->
    is_valid_from_tuple_check(NS, Name, Def);
is_valid_from_tuple({NS, Name, disable}) when is_atom(NS), is_atom(Name) ->
    is_valid_from_tuple_check(NS, Name, disable);
is_valid_from_tuple(Rule) when is_record(Rule, rule) ->
    {true, Rule};
is_valid_from_tuple(_) ->
    {false, "got an invalid tuple (is def. a map or 'disable'?)."}.

is_valid_from_tuple_check(NS, Name, Def) ->
    _ = maybe_ensure_loaded(NS),
    ArityForExecute = 2,
    case erlang:function_exported(NS, Name, ArityForExecute) of
        true ->
            {true, from_tuple({NS, Name, Def})};
        false ->
            case elvis_deprecated_rules:find(NS, Name) of
                valid ->
                    {false,
                        io_lib:format("got an unexpected/invalid ~p:~p/~p combo.", [
                            NS, Name, ArityForExecute
                        ])};
                {_, Msg} ->
                    {deprecated, Msg}
            end
    end.

maybe_ensure_loaded(NS) when not is_atom(NS) ->
    ok;
maybe_ensure_loaded(NS) ->
    code:ensure_loaded(NS).

-spec is_ignorable(term()) -> boolean().
% String (file path / regex pattern)
is_ignorable(String) when is_list(String) ->
    io_lib:char_list(String) andalso length(String) > 0;
% Module - invalid type
is_ignorable(Module) when not is_tuple(Module) andalso not is_atom(Module) ->
    false;
% Module - test if valid
is_ignorable(Module) when is_atom(Module) ->
    case maybe_ensure_loaded(Module) of
        {module, _} ->
            true;
        _ ->
            false
    end;
% {Module, Function} - invalid type
is_ignorable({Module, Function}) when not is_atom(Module) orelse not is_atom(Function) ->
    false;
% {Module, Function} - test if valid
is_ignorable({Module, Function}) ->
    case is_ignorable(Module) of
        true ->
            Exports = Module:module_info(exports),
            proplists:get_value(Function, Exports) =/= undefined;
        false ->
            false
    end;
% {Module, Function, Arity} - invalid type
is_ignorable({Module, Function, Arity}) when
    not is_atom(Module) orelse not is_atom(Function) orelse not is_integer(Arity) orelse Arity < 0
->
    false;
% {Module, Function, Arity} - test if valid
is_ignorable({Module, Function, Arity}) ->
    case is_ignorable(Module) of
        true ->
            Exports = Module:module_info(exports),
            proplists:get_value(Function, Exports) =:= Arity;
        false ->
            false
    end.

-spec ns(t()) -> module().
ns(Rule) ->
    Rule#rule.ns.

-spec name(t()) -> atom().
name(Rule) ->
    Rule#rule.name.

-spec def(t()) -> def().
def(Rule) ->
    Rule#rule.def.

-spec ignores(t()) -> [ignorable()].
ignores(Rule) ->
    Rule#rule.ignores.

-spec disabled(t()) -> boolean().
disabled(Rule) ->
    Rule#rule.disabled.

-spec file(t()) -> elvis_file:t().
file(Rule) ->
    Rule#rule.file.

-spec file(t(), elvis_file:t()) -> t().
file(Rule, File) ->
    Rule#rule{
        file = File
    }.

-spec disable(t()) -> t().
disable(Rule) ->
    Rule#rule{
        disabled = true
    }.

-spec ignored(Needle :: ignorable(), t()) -> boolean().
ignored(Needle, Rule) ->
    lists:member(Needle, ignores(Rule)).

-spec execute(t(), ElvisConfig) -> Results when
    ElvisConfig :: elvis_config:t(),
    Results :: [elvis_result:rule() | elvis_result:elvis_error()].
execute(#rule{rule_fun = RuleFun} = Rule, ElvisConfig) ->
    RuleFun(Rule, ElvisConfig).

-spec option(Key :: atom(), t()) -> Value :: undefined | term().
option(Key, Rule) ->
    case maps:get(Key, Rule#rule.def, undefined) of
        undefined ->
            maps:get(Key, default(Rule), undefined);
        CurValue ->
            CurValue
    end.

-spec default(t()) -> def().
default(Rule) ->
    NS = Rule#rule.ns,
    Name = Rule#rule.name,
    default(NS, Name).

-spec default(NS :: module(), Name :: atom()) -> def().
default(NS, Name) ->
    _ = maybe_ensure_loaded(NS),
    ArityForDefault = 1,
    case erlang:function_exported(NS, default, ArityForDefault) of
        false ->
            #{};
        true ->
            NS:default(Name)
    end.

-spec defmap(map()) -> def().
defmap(Map) ->
    Map.

-spec defkeys(t()) -> [atom()].
defkeys(Rule) ->
    Def = def(Rule),
    maps:keys(Def).

-spec ignorable(module() | {module(), atom()} | {module(), atom(), arity()}) -> ignorable().
ignorable(Ignorable) ->
    Ignorable.

-spec same(t(), t()) -> boolean().
%% @doc Loose equality comparison for a specific use case
same(RuleL, RuleR) ->
    {ns(RuleL), name(RuleL)} =:= {ns(RuleR), name(RuleR)}.
