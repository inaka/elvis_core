-module(pass_no_macros).
-define(ALLOWED_MACRO, allowed_macro).
-export([allowed_macro/0]).

allowed_macro() -> ?ALLOWED_MACRO.
