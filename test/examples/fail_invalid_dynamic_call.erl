-module(fail_invalid_dynamic_call).

-ignore_xref({normal, call, 0}).
-ignore_xref({another_normal, call, 0}).

-dialyzer(no_match).

-export([
         dynamic_module_name_call/0,
         dynamic_function_name_call/0,
         another_dynamic_module_name_call/0,
         dynamic_module_name_call_in_case/0,
         dynamic_module_name_call_in_try_of/0,
         dynamic_module_name_call_in_try_catch/0,
         dynamic_module_name_call_in_catch/0
        ]).

dynamic_module_name_call() ->
    normal:call(),
    Module = a_module,
    Module:call().

dynamic_function_name_call() ->
    normal:call(),
    Function = a_function,
    a_module:Function(),
    normal:call().

another_dynamic_module_name_call() ->
    normal:call(),
    another_normal:call(),
    Module = another_module,
    Module:call_to_function(),
    Module:call_to__another_function().

dynamic_module_name_call_in_case() ->
    normal:call(),
    another_normal:call(),
    case 1 of
        1 ->
            Module = another_module,
            Module:call_to_function();
        2 -> ok
    end.

dynamic_module_name_call_in_try_of() ->
    normal:call(),
    another_normal:call(),
    Module = yam,
    try Module:call_to_function() of
        a -> ok
    catch _:_ ->
        ok
    end.

dynamic_module_name_call_in_try_catch() ->
    normal:call(),
    another_normal:call(),
    Module = yamm,
    try
        Module:call_to_function()
    catch _:_ ->
        ok
    end.

dynamic_module_name_call_in_catch() ->
    Module = yammm,
    catch Module:call_to_function().
