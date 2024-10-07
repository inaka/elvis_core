# No Init Lists 

This warns you if you use list as a parameter in an init function in a gen_* module.
[Reasoning](https://erlangforums.com/t/args-in-gen-init-1/3169/4?u=elbrujohalcon)

> Works on `.beam` file? Not really! (it consumes results Ok, but these might be unexpected, since
the files are pre-processed)

## Options

- None.

## Example

```erlang
{elvis_style, no_init_lists, #{}}
```
