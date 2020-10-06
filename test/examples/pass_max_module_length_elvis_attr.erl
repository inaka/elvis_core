-module(pass_max_module_length_elvis_attr).

-elvis([{elvis_style, max_module_length, #{max_length => 19, count_comments => true, count_whitespace => true}}]).
-elvis([{elvis_text_style, line_length, disable}]).

-export([f/1]).




%% Random comment





%% @doc A function
f(_) -> ok.
