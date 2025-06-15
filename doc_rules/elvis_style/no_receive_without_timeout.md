# No `receive` Without Timeout [![](https://img.shields.io/badge/since-4.1.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.1.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

All `receive` expressions should be accompanied by an `after` expressions.

## Avoid

```erlang
receive
    something ->
        do:something()
end
```

## Prefer

```erlang
receive
    something ->
        do:something()
after
    60_000 ->
        exit(nothing_received)
end
```

## Rationale

A `receive` block without a timeout will wait indefinitely if no matching message arrives; by making
your timeout explicit you:

- avoid hanging processes.
- improve testability (deterministic behavior under test failure conditions).
- ease debug and recovery.
- can implement retry and self-healing.
- potentially avoid denial-of-service scenarios where waits are exploited.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_receive_without_timeout, #{}}
```
