-module(pass_macro_names_elvis_attr).

 -define (bad_name,   "(no)").
-define(GOOD_NAME,  "(megusta)").
-define(wtf_NAME,   "(yuno)").
-define(GOOD_NAME(Arg), "(megusta)").
-define(GOOD_NAME_TOO (Arg), "(megusta)").
-define(  'POTENTIAL_BAD-NAME'  , nomegusta).
-define('A,aZ', 2).

-elvis([{elvis_style, macro_names, #{regex => "^[a-zA-Z_,\-]+$"}}]).
-elvis([{elvis_style, no_space, #{rules => []}}]).

-export([
         define/1,
         use_define/1
        ]).

define(not_a_macro) -> its_not.
define(_Also, not_a_macro) -> still_not.

%% this is not using -define
use_define(x) ->
  define(this_is, not_a_macro),
  Not = "-define(this_isnt, a_macro)",
  Not.
