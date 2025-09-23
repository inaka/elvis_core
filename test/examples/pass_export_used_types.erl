-module pass_export_used_types.

-define(TYPE_CONSTANT, type_constant).

-type my_type() :: my | type.
-type private_type(X) :: {X}.
-export_type [my_type/0].

-export [my_fun/1, fun_using_type_constant/0].

-spec my_fun(my_type()) -> my_type().
my_fun(my) -> type;
my_fun(type) ->
    private_fun(none),
    my.

% ignore types only used by private functions
-spec private_fun(X) -> private_type(X).
private_fun(none) -> {none}.

-spec fun_using_type_constant() -> ?TYPE_CONSTANT.
fun_using_type_constant() ->
    ?TYPE_CONSTANT.
