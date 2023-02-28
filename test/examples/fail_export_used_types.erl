-module fail_export_used_types.

-type my_type() :: my | type.

-export [my_fun/1].

-spec my_fun(my_type()) -> my_type().
my_fun(my) -> type;
my_fun(type) -> my.
