-module(fail_macro_module_names).

-dialyzer({nowarn_function, [function_name/0, build_binary/0]}).

-export([
         module_name/0,
         function_name/0,
         no_errors/0,
         build_binary/0,
         no_errors2/0
        ]).

-define(FUN_NAME, function_name).
-define(BINARY, "bla").
-define(BINARY_SIZE, 3).
-define(function_name, function_name).
-define(module_name, ?MODULE).

-define(MATCH_MY_ERROR, {my_error, _, _}).
-define(MATCH_MY_ERROR(ErrorTag), {my_error, ErrorTag, _}).

module_name() ->
    ?MODULE:function_name(),
    ?module_name:?function_name(),
    ?module_name:function_name(with, arguments),
    ?MODULE:?function_name(with, arguments),
    ?module_name:?function_name(with, arguments),
    ?function_name(with, arguments).

function_name() ->
    module:?FUN_NAME(params),
    module:?FUN_NAME (params),
    lists:?MODULE().

function_name(with, arguments) -> "should fail anyway".

build_binary() ->
    Bin = <<?BINARY:32>>,
    _Bin2 = <<Bin:?BINARY_SIZE/binary>>,
    _Bin3 = <<?BINARY:?BINARY_SIZE>>,
    BinSize = ?BINARY_SIZE,
    _Bin4 = <<?BINARY:BinSize>>,
    <<Bin, ?BINARY:1, "prefix">>.

no_errors() ->
    ?LINE.

no_errors2() ->
    try other:my_logick()
    catch
        throw:?MATCH_MY_ERROR(bad_auth) ->
            handle_auth_error;
        throw:?MATCH_MY_ERROR(input_validation) ->
            handle_validation_error;
        throw:?MATCH_MY_ERROR ->
            handle_other_application_errors
    end.
