# Param Pattern-Matching

(since [3.0.0](https://github.com/inaka/elvis_core/releases/tag/3.0.0))

When capturing a parameter using pattern matching you can either put the parameter
name on the left (`Param = #{pattern := ToMatch}`) or right (`#{pattern := ToMatch} = Param`) side
of the pattern that you use in the function clause.
This rule will make sure you are consistent through your code and use always the same style.

> Works on `.beam` file? Yes!

## Options

- `side :: left | right`.
  - default: `left`.

## Example

```erlang
{elvis_style, param_pattern_matching, #{side => right}}
```
