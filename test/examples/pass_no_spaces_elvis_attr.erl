-module(pass_no_spaces_elvis_attr).

-elvis([{elvis_text_style, no_spaces, disable}]).
-elvis([{elvis_style, no_space, disable}]).
-elvis([{elvis_text_style, no_tabs, disable}]).

-export([one/0, two/0, three/0, four/0, five/0]).

-ignore_xref([{x, this_line_is_good, 0}]).
-ignore_xref([{x, this_line_is_wrong, 0}]).
-ignore_xref([{x, this_line, 2}]).
-ignore_xref([{x, this_line, 3}]).
-ignore_xref([{x, fail, 1}]).
-ignore_xref([{x, this_line_should, 1}]).
-ignore_xref([{x, this_line_should, 3}]).

one() ->
  not_ok. %%This lines has a spaces

two() ->
		ok. %%This one doesn't

three() ->
	ok. %% This one doesn't

four() ->
    not_ok. %% This lines has 4 spaces

five() ->
				x:this_line_is_good(),
				 x:this_line_is_wrong(),
								x:this_line(is, good),
								x:this_line(is, also,   good),
								x:this_line(should,  nott,      x:fail( either )),
        x:this_line_should(fail),
							  x:this_line_should(fail, as, well).
