# No Init Lists

Do not use a list as the parameter for the `init/1` callback when implementing `gen_*` behaviours. It's semantically clearer to use a tuple or a map. [More info](https://erlangforums.com/t/args-in-gen-init-1/3169/5)

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected, since
the files are pre-processed)

## Options

- None.

## Example

```erlang
{elvis_style, no_init_lists, #{}}
```
