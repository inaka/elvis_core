# No Successive Maps

(since [2.0.0](https://github.com/inaka/elvis_core/releases/tag/2.0.0))

The idea behind this rule comes from [this email](http://erlang.org/pipermail/erlang-questions/2017-April/092112.html) by @kvakvs.
Basically, the warning is emitted if a developer _forgets a comma_ and writes something like the following:

```erlang
[#{a => 1}
 #{b => 2}]
```

It will also warn on things like ``#{a =>1}#{b => 2}``, which is easily rewritable as ``#{a => 1, b => 2}``.

> Works on `.beam` file? Yes!

## Options

- None.

## Example

```erlang
{elvis_style, no_successive_maps}
%% or
{elvis_style, no_successive_maps, #{}}
```
