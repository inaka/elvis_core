# State Record and Type

Every module that implements an OTP behavior in the following list should have a `state` record
(`#state{}`) and a `state` type (`type()`) or a `state` private type (`opaque()`):

- `gen_server`
- `gen_event` (since [0.7.0](https://github.com/inaka/elvis_core/releases/tag/0.7.0))
- `gen_fsm`
- `supervisor_bridge`

If enabled with `export_used_types` together, the `state` record should be defined as a private
type (`opaque()`), and should be exported.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, state_record_and_type}
%% or
{elvis_style, state_record_and_type, #{}}
```
