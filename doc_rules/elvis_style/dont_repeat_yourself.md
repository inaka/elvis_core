# Don't Repeat Yourself

The *Don't Repeat Yourself* ([DRY](https://en.wikipedia.org/wiki/Don't_repeat_yourself)) rule checks
if there is repeated code within a module. A piece of code is considered repeated or duplicated when
its structure can be found in at least another piece of code in the same module.

The `min_complexity` option determines the level of complexity a structure should have to be
considered as repeated. For example, a simple list concatenation of two variables (i.e., `X ++ Y`)
is a fairly simple expression that could be used any number of times within a module. Still, it
wouldn't be correct to report it as repeated code. Only more complex expressions like an entire
`case` structure would qualify as repeated code when all its clauses have the exact same code.

It's possible to tune the `min_complexity` parameter to what you consider correct by performing a
trial and error process of setting a value, checking what is reported, and repeating.

> Works on `.beam` file? Yes!

## Options

- `min_complexity :: non_neg_integer()`.
  - default: `10`.

## Example

```erlang
{elvis_style, dont_repeat_yourself}
%% or
{elvis_style, dont_repeat_yourself, #{ min_complexity => 10 }}
```
