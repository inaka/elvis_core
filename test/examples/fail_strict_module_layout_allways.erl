-module(fail_strict_module_layout_allways).

-type bananas() :: bananas.
-export_type([bananas/0, tomatoes/0]).
-type tomatoes() :: tomatoes.

