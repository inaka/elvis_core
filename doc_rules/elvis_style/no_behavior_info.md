# No Behavior Info

The use of `behaviour_info` (or `behavior_info`) attributes should be avoided; use `-callback`
annotations instead.

> Works on `.beam` file? Yes!

## Avoid

```erlang
-module(my_behavior).

-export([behavior_info/1]).

behavior_info(callbacks) ->
    [{init, 1}, {handle_call, 3}, {terminate, 2}];
behavior_info(_) ->
    undefined.
```

## Prefer

```erlang
-module(my_behavior).

-callback init(Args :: term()) -> {ok, State :: term()}.
-callback handle_call(Request :: term(), From :: pid(), State :: term()) -> Result :: any().
-callback terminate(Reason :: term(), State :: term()) -> ok.
```

## Rationale

The `-callback` attribute is the modern and preferred way to define behavior callbacks in Erlang.
The older `behavior_info`/`behaviour_info` attributes are deprecated and should be avoided to
ensure forward compatibility and better tool support.

## Options

- None.

## Example

```erlang
{elvis_style, no_behavior_info, #{}}
```
