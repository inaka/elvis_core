# No Space after `#`

There should be no spaces after the `#` symbol for maps nor records.

In Erlang, using spaces after the `#` symbol when dealing with maps or records, is valid.
Still, it should be discouraged.

## Examples

```
BadMap = #   {this => map, has => spaces, after => pound}.
BadRecord =     #      this_record{has = spaces, after = pound}.

GoodMap = #{this => map, has => no_spaces, after => pound}.
GoodRecord =     #this_record{has = no_spaces, after = pound}.
```

> Works on `.beam` file? No.

## Options

- None

## Example

```erlang
{elvis_style, no_space_after_pound}
```
