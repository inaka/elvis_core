-module(elvis_rule).

-export([
    new/2, new/3,
    from_tuple/1,
    is_valid_from_tuple/1,
    ns/1,
    name/1,
    def/1,
    ignores/1,
    disabled/1,
    file/1, file/2,
    ignored/2,
    execute/2,
    option/2,
    defmap/1,
    ignorable/1,
    same/2
]).

-record(rule, {
    ns :: module(),
    name :: atom(),
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

is_valid_from_tuple(Tuple) ->
    case from_tuple(Tuple) of
        invalid_tuple ->
            {false, "got an invalid tuple (is def. a map or 'disable'?)."};
        Rule ->
            NS = ns(Rule),
            case is_atom(NS) of
                true ->
                    _ = code:ensure_loaded(NS);
                false ->
                    ok
            end,
            Name = name(Rule),
            ArityForExecute = 2,
            case
                is_atom(NS) andalso is_atom(Name) andalso
                    erlang:function_exported(NS, Name, ArityForExecute)
            of
                true ->
                    {true, Rule};
                _ ->
                    {false,
                        io_lib:format("got an unexpected/invalid ~p:~p/~p combo.", [
                            NS, Name, ArityForExecute
                        ])}
            end
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
    lists:member(Needle, Rule#rule.ignores).

-spec execute(t(), ElvisConfig) -> Results when
    ElvisConfig :: elvis_config:t(),
    Results :: [elvis_result:rule() | elvis_result:elvis_error()].
execute(Rule, ElvisConfig) ->
    NS = Rule#rule.ns,
    Name = Rule#rule.name,
    NS:Name(Rule, ElvisConfig).

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
    NS:default(Name).

-spec defmap(map()) -> def().
defmap(Map) ->
    Map.

-spec ignorable(module() | {module(), atom()} | {module(), atom(), arity()}) -> ignorable().
ignorable(Ignorable) ->
    Ignorable.

-spec same(t(), t()) -> boolean().
%% @doc Loose equality comparison for a specific use case
same(RuleL, RuleR) ->
    {ns(RuleL), name(RuleL)} =:= {ns(RuleR), name(RuleR)}.
