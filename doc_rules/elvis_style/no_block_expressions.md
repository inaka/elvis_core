# No Block Expressions [![](https://img.shields.io/badge/since-1.4.0-blue)](https://github.com/inaka/elvis_core/releases/tag/1.4.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Block expressions should be avoided.

## Avoid

```erlang
check(Vehicle, Height, Age) ->
    io:format("Let's check the height"),
    Height > 140 andalso
        begin
            io:format("Let's check the age"),
            Age > 12 andalso
                begin
                    io:format("Height and age check out. Vehicle?"),
                    Vehicle =:= car
                end
        end.
```

## Prefer

An example refactoring (no performance considerations taken into account) could be:

```erlang
check(Vehicle, Height, Age) ->
    Checks = [check_height(Height),
              check_age(Age),
              check_is_car(Vehicle)],
    not(lists:member(false, Checks)).

check_height(Height) ->
    Res = Height > 140,
    not(Res) andalso io:format("Insuffient height"),
    Res.

check_age(Age) ->
    Res = Age > 12,
    not(Res) andalso io:format("Insuffient age"),
    Res.

check_is_car(Vehicle) ->
    Res = Vehicle =:= car,
    not(Res) andalso io:format("Not a car"),
    Res.
```

## Rationale

Block expressions often introduce unnecessary indentation, making code harder to read and
understand. By avoiding them, the code becomes flatter, cleaner, and more consistent with Erlang’s
functional style. Reducing the use of block expressions helps keep functions focused and improves
the maintainability of the code. In most cases, code inside blocks can be refactored into more
straightforward patterns, improving clarity and reducing the potential for errors.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_block_expressions, #{}}
```
