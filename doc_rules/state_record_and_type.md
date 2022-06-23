# State Record and Type

Every module that implements an OTP behavior in the following list should have a `state` record
(`#state{}`) and a `state` type (`type()`):

- `gen_server`
- `gen_event` (since [0.7.0](https://github.com/inaka/elvis_core/releases/tag/0.7.0))
- `gen_fsm`
- `supervisor_bridge`

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, state_record_and_type}
%% or
{elvis_style, state_record_and_type, #{}}
```
