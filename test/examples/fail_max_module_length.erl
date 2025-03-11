-module(fail_max_module_length).
-if(?OTP_RELEASE >= 27).
-moduledoc false.
-endif.
-export([f/1]).




%% Random comment





%% @doc A function.
-if(?OTP_RELEASE >= 27).
-doc """
A function.
""".
-doc(#{since => "1.0"}).
-endif.
f(_) -> ok.
