# Migration helper

This file presents the changes required to your consumption code/configuration when you bump
from a given version to a new one, as presented below.
If you're bumping more than a single version, be sure to respect all the intermediate instructions,
since these are incremental.

This file's format is influenced by [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and
[Make a README](https://www.makeareadme.com/).

## Going from `3.x` to `4.x`

### Update

- your `atom_naming_convention`'s options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/atom_naming_convention.md#options>
- your `macro_names`' options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/macro_names.md#options>
- your `operator_spaces` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/operator_spaces.md#options>
- your `no_space` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/no_space.md#options>
- your `function_naming_convention` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/function_naming_convention.md#options>
- your `module_naming_convention` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/module_naming_convention.md#options>
- your `no_debug_call` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/no_debug_call.md#options>
- your `no_common_caveats_call` options to be the ones defined at
<https://github.com/inaka/elvis_core/blob/3.2.5/doc_rules/elvis_style/no_common_caveats_call.md#options>

On the other hand you may choose to not implement those changes and merely adapt your code.

## Going from `0.x` to `1.x`

### Update

- your `ignore` option, from rule `max_function_length`, to also contain the content of
`ignore_functions`
- your rule configuration files (e.g. `elvis.config`), by moving `elvis_style` to
`elvis_text_style` for the following rules: `line_length`, `no_tabs`, and `no_trailing_whitespace`

### Delete

- option `ignore_functions`, from rule `max_function_length`
