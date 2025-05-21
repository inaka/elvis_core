# Don't Repeat Yourself

The *Don't Repeat Yourself* ([DRY](https://en.wikipedia.org/wiki/Don't_repeat_yourself)) rule
identifies repeated code within a module. Code is considered repeated or duplicated if its
structure appears in at least one other location within the same module.

The `min_complexity` option defines the complexity threshold for a structure to be considered
repeated. For instance, a simple list concatenation like `X ++ Y` is a basic expression that can
appear multiple times without being flagged as duplication. However, more complex
structures - such as an entire case expression where all clauses contain identical code - would
qualify as repeated.

The `min_complexity` parameter can be adjusted according to your needs through a trial-and-error
process: set a value, review the reported results, and adjust accordingly.

> Works on `.beam` file? Yes!

## Avoid

This example is for `min_complexity` 1.

```erlang
process_order(OrderTotal) ->
    DiscountedTotal = OrderTotal - (OrderTotal * 0.1),
                    % ^_what to avoid_______________^
    io:format("Order total after discount: ~p~n", [DiscountedTotal]),
    DiscountedTotal.

process_invoice(InvoiceTotal) ->
    DiscountedTotal = InvoiceTotal - (InvoiceTotal * 0.1),
                    % ^_what to avoid___________________^
    io:format("Invoice total after discount: ~p~n", [DiscountedTotal]),
    DiscountedTotal.
```

## Prefer

Note that we replace the *discount* implementation by a single function, where we previously had 2.

```erlang
process_order(OrderTotal) ->
    DiscountedTotal = apply_discount(OrderTotal),
    io:format("Order total after discount: ~p~n", [DiscountedTotal]),
    DiscountedTotal.

process_invoice(InvoiceTotal) ->
    DiscountedTotal = apply_discount(InvoiceTotal),
    io:format("Invoice total after discount: ~p~n", [DiscountedTotal]),
    DiscountedTotal.

apply_discount(Amount) ->
    Amount - (Amount * 0.1).
```

## Rationale

Code duplication is a significant issue in software development for several reasons:

- **Harder to maintain**: repeated logic needs to be updated in multiple locations
- **increased risk of bugs**: more code increases the likelihood of bugs, some of which may be
harder to detect and fix
- **code bloat**: unnecessary code growth makes the codebase larger, harder to read, and more
difficult to navigate
- **harder to test**: when logic is scattered across the codebase, validation becomes more complex,
and achieving high test coverage becomes more challenging

## Options

- `min_complexity :: non_neg_integer()`
  - default: `10`

## Example

```erlang
{elvis_style, dont_repeat_yourself, #{ min_complexity => 10 }}
```
