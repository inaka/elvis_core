-module(fail_macro_names).

 -define (bad_name,   "(no)").
 -define (error(A),   {error, A}).
-define(GOOD_NAME,  "(megusta)").
-define(wtf_NAME,   "(yuno)").
-define(GOOD_NAME(Arg), "(megusta)").
-define(GOOD_NAME_WITH_ATOM_RESULT(Arg), some_atom).
-define(GOOD_NAME_WITH_VAR_RESULT(Arg), Arg).
-define(GOOD_NAME_TOO (Arg), "(megusta)").
-define(CALLING_SOMETHING, call(1)).
-define(  'POTENTIAL_BAD-NAME'  , nomegusta).
-define('A,aZ', 2).
-define (BAD__NAME, "Stumble").

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
