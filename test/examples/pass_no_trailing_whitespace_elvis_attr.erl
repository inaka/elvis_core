-module(pass_no_trailing_whitespace_elvis_attr).

-export([one/0, two/0, three/0, four/0, five/0]).
-export([six/0, seven/0, eight_and_rocking_alright/0]).

-elvis([{elvis_text_style, no_trailing_whitespace, disable}]).
-elvis([{elvis_text_style, no_tabs, disable}]).

one() ->
    %% Following line ends with a tab
    not_ok.	

two() ->
    %% Following line ends with a space
    not_ok. 

three() -> 
    %% Previous line ends with a space
    not_ok.
 
four() -> %% Previous (supposedly blank) line has a space
    not_ok.

five() ->
    ok. %% This function should be fine

six() ->
    %% Following line ends with a Windows newline ("\r\n")
    ok.

seven() ->
    %% Previous line ends with a Windows newline ("\r\n")
    ok.

%% Previous (supposedly blank) line has a Windows newline ("\r\n")
eight_and_rocking_alright() ->
    ok.
