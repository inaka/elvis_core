-module(fail_max_module_length).
-moduledoc false.
-export([f/1]).




%% Random comment





%% @doc A function.
-doc """
A function.
""".
-doc(#{since => "1.0"}).
f(_) -> ok.