-module(user_defined_rules).

-export([rule/3]).

rule(_Config, _Target, _) ->
    [elvis_result:new_item("this will always FAIL", [], #{line => 10, column => 2})].
