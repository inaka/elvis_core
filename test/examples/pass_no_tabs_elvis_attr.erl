-module(pass_no_tabs_elvis_attr).

-export([one/0, two/0, three/0, four/0]).

-elvis([{elvis_text_style, no_tabs, disable}]).

one() ->
	not_ok. %%This lines has a tab

two() ->
    ok. %%This one doesn't

three() ->
 ok. %% This one doesn't

four() ->
		not_ok. %% This lines has 2 tabs
