# Include `ms_transform` for `ets:fun2ms/1` [![](https://img.shields.io/badge/since-4.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/4.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

The `ms_transform` parse transform should be included when the module uses `ets:fun2ms`.

## Avoid

```erlang
    Fun = fun(Emp) -> Emp#emp.empno end),
    ets:select(emp_tab, ets:fun2ms(Fun)).
```

## Prefer

```erlang
-include_lib("stdlib/include/ms_transform.hrl").
% ^ include this

...

    Fun = fun(Emp) -> Emp#emp.empno end),
    ets:select(emp_tab, ets:fun2ms(Fun)).
```

## Rationale

The `fun2ms/1` function relies on compile-time transformation provided by `ms_transform`. Without
including this parse transform, the compilation will fail or the function will not behave as
expected.

## Options

- None.

## Example configuration

```erlang
{elvis_style, ms_transform_included, #{}}
```
