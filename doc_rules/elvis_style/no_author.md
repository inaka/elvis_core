# No Author [![](https://img.shields.io/badge/since-1.5.0-blue)](https://github.com/inaka/elvis_core/releases/tag/1.5.0) ![](https://img.shields.io/badge/BEAM-yes-orange)

The use of attribute `-author(_)` should be avoided.

## Avoid

```erlang
-author("john.doe@ericsson.com").
```

## Prefer

```erlang
% nothing to replace it with
```

## Rationale

In modern Erlang, this practice serves no specific purpose.

## Reference

[Paraphrasing @aboroska...](https://github.com/inaka/elvis_core/issues/149#issuecomment-699679362):

> the custom of adding an `author` attribute came from Ericsson with the intention to help
operations/testers to call/notify the right developers when a crash happened.

## Options

- None.

## Example configuration

```erlang
{elvis_style, no_author, #{}}
```
