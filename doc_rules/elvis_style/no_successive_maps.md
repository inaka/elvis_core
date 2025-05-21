# No Successive Maps [![](https://img.shields.io/badge/since-2.0.0-blue)](https://github.com/inaka/elvis_core/releases/tag/2.0.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

Successive map expressions should be avoided.

## Avoid

```erlang
[#{a => 1}
 #{b => 2}]
```

## Prefer

```erlang
[#{a => 1,
   b => 2}].
```

## Rationale

While writing successive map expressions like `#{a => 1} #{b => 2}` is syntactically valid, it is
almost always a sign of a mistake or unclear intent. Erlang treats these as the same expression,
which may confuse readers or mislead them into thinking the maps are two expressions. To ensure
clarity and correctness, merge the map expressions as in the example provided in "Prefer".

## Reference

The idea behind this rule comes from
[this email](https://erlang.org/pipermail/erlang-questions/2017-April/092112.html) by @kvakvs.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_successive_maps, #{}}
```
