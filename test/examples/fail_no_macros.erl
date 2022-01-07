-module(fail_no_macros).
-define(FORBIDDEN_MACRO, forbidden_macro).
-export([forbidden_macro/0]).

forbidden_macro() -> ?FORBIDDEN_MACRO.
