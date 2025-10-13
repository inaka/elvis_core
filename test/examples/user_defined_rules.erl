-module(user_defined_rules).

-export([rule/2]).

rule(_Config, _Target) ->
    [elvis_result:new_item("this will always FAIL", [], #{line => 10, column => 2})].
