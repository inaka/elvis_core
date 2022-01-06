-module(pass_macro_module_names_elvis_attr).

-dialyzer({nowarn_function, [function_name/0, build_binary/0]}).

-elvis([{elvis_style, macro_module_names, disable}]).
-elvis([{elvis_style, macro_names, #{regex => "^[a-zA-Z_]+$"}}]).
-elvis([{elvis_style, no_macros, #{allow => [ function_name
                                            , module_name
                                            , 'FUN_NAME'
                                            , 'BINARY'
                                            , 'BINARY_SIZE'
                                            ]
                                  }}]).

-export([
         module_name/0,
         function_name/0,
         no_errors/0,
         build_binary/0
        ]).

-ignore_xref({pass_macro_module_names_elvis_attr, function_name, 0}).
-ignore_xref({module, function_name, 1}).
-ignore_xref({lists, pass_macro_module_names_elvis_attr, 0}).

-define(FUN_NAME, function_name).
-define(BINARY, "bla").
-define(BINARY_SIZE, 3).
-define(function_name, function_name).
-define(module_name, ?MODULE).

module_name() ->
    ?MODULE:function_name(),
    ?module_name:?function_name().

function_name() ->
    module:?FUN_NAME(params),
    module:?FUN_NAME (params),
    lists:?MODULE().

build_binary() ->
    Bin = <<?BINARY:32>>,
    _Bin2 = <<Bin:?BINARY_SIZE/binary>>,
    _Bin3 = <<?BINARY:?BINARY_SIZE>>,
    BinSize = ?BINARY_SIZE,
    _Bin4 = <<?BINARY:BinSize>>,
    <<Bin, ?BINARY:1, "prefix">>.

no_errors() ->
    ?LINE.
