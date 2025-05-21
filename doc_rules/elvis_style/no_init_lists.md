# No Init Lists

(since [4.0.0](https://github.com/inaka/elvis_core/releases/tag/4.0.0))

Lists as the argument for the `init/1` callback should be avoided when implementing
`gen_*` behaviours; use a tuple, a map, or a record instead.

> Works on `.beam` files? Yes!

## Avoid

```erlang
-behaviour(gen_server).

...

start_link(Name, Timeout) ->
    gen_server:start_link(?MODULE, _Args = [Name, Timeout], _Options = []).

init([Name, Timeout]) ->
    ...
```

## Prefer

Using a map as an example:

```erlang
-behaviour(gen_server).

...

start_link(Name, Timeout) ->
    gen_server:start_link(?MODULE, _Args = #{name => Name, timeout => Timeout}, _Options = []).

init(#{ name := Name, timeout := Timeout }) ->
    ...
```

## Rationale

When implementing `gen_server`, `gen_statem`, or other `gen_*` behaviours, using a list as the
argument to the `init/1` callback is discouraged. Tuples, maps, and records provide clearer
structure, better pattern matching, and improved readability.
Lists are typically used for variable-length collections and may lead to ambiguous or less
maintainable code in this context.

## Reference

[Erlang Forums - Args in gen_*:init/1](https://erlangforums.com/t/args-in-gen-init-1/3169/5)

## Options

- `behaviours :: [atom()]` - list the behaviours for which you want to apply the rule
  - default: `[gen_server, gen_statem, gen_fsm, supervisor, supervisor_bridge, gen_event]`

## Example configuration

```erlang
{elvis_style, no_init_lists, #{ behaviours => [gen_server
                                             , gen_statem
                                             , gen_fsm
                                             , supervisor
                                             , supervisor_bridge
                                             , gen_event
                                              ]
                              }}
```
