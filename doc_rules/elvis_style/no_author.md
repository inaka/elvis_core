# No author

(since [1.5.0](https://github.com/inaka/elvis_core/releases/tag/1.5.0))

> Works on `.beam` file? Yes!

## Problematic code

```erlang
-author("john.doe@ericsson.com").
```

## Correct code

```erlang
% nothing to replace it with
```

## Rationale

[Paraphrasing @aboroska...](https://github.com/inaka/elvis_core/issues/149#issuecomment-699679362):

> the custom of adding an `author` attribute came from Ericsson with the intention to help
operations/testers to call/notify the right developers when a crash happened.

In modern Erlang, this practice serves no specific purpose.

## Options

- None.

## Example

```erlang
{elvis_style, no_author, #{}}
```
