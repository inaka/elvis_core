-module(pass_invalid_dynamic_call_callback).

-callback call() -> _.
-callback macro_function_name_call() -> _.

-define(MACRO_CALL(M, F, A), M:F(A)).

-define(A_MODULE, a_module).

-export([
         variable_module_name_call/1,
         variable_function_name_call/2,
         macro_module_name_call/0,
         macro_function_name_call/0,
         call_module_name_call/0,
         call_function_name_call/0,
         macro_call/3
        ]).

variable_module_name_call(Module) ->
    Module:call().

variable_function_name_call(Module, Function) ->
    module:Function(),
    Module:Function().

macro_module_name_call() ->
    ?A_MODULE:call().

macro_function_name_call() ->
    module:?FUNCTION_NAME(),
    ?A_MODULE:?FUNCTION_NAME().

call_module_name_call() ->
    (get:the_module()):call().

call_function_name_call() ->
    module:(get:the_function())(),
    (get:the_module()):(get:the_function())().

%% @doc This one only fails in beam files.
macro_call(M, F, A) ->
    ?MACRO_CALL(M, F, A).
