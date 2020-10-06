# Migration helper

This file presents the changes required to your consumption code/configuration when you bump
from a given version to a new one, as presented below.
If you're bumping more than a single version, be sure to respect all the intermediate instructions,
since these are incremental.

This file's format is influenced by [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and
[Make a README](https://www.makeareadme.com/).

## Going from `0.7.0` to `1.0.0`

### Update

- your `ignore` option (from rule `max_function_length`) to also contain the content of
`ignore_functions`
- your rule config. files (e.g. `elvis.config`), by moving `elvis_style` to `elvis_text_style` for
the following rules: `line_length`, `no_tabs`, and `no_trailing_whitespace`

### Delete

- option `ignore_functions` from rule `max_function_length`
