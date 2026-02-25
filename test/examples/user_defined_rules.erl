-module(user_defined_rules).
-behaviour(elvis_rule).

-export([default/1, rule/2]).

default(rule) ->
    elvis_rule:defmap(#{});
default(_) ->
    elvis_rule:defmap(#{}).

rule(_Rule, _ElvisConfig) ->
    [elvis_result:new_item("this will always FAIL", [], #{line => 10, column => 2})].
