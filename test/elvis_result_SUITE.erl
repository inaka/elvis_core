-module(elvis_result_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([without_line/1]).
-export([without_column/1]).
-export([wihout_limit/1]).
-export([without_data/1]).
-export([with_attrs_for_data/1]).
-export([complete/1]).

-include_lib("stdlib/include/assert.hrl").

all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, elvis_test_utils:excluded_funs_all())].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(elvis_core),
    Config.

end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

without_line(_Config) ->
    Rule = god_modules,
    RuleConfig = #{limit => 1},
    ExampleTestFile = "elvis_results_new_item.erl",

    [#{message := Message, line_num := LineNum, column_num := ColumnNum, info := [9]}] =
        elvis_test_utils:elvis_core_apply_rule(
            _TestcaseConfig = [],
            elvis_style,
            Rule,
            RuleConfig,
            ExampleTestFile
        ),

    ?assertEqual(
        "This module's function count (~p) is higher than the configured limit (limit: 1).", Message
    ),
    ?assertEqual(-1, LineNum),
    ?assertEqual(-1, ColumnNum).

without_column(_Config) ->
    Rule = no_redundant_blank_lines,
    RuleConfig = #{},
    ExampleTestFile = "elvis_results_new_item.erl",

    [#{message := Message, line_num := LineNum, column_num := ColumnNum, info := [2]}] =
        elvis_test_utils:elvis_core_apply_rule(
            _TestcaseConfig = [],
            elvis_text_style,
            Rule,
            RuleConfig,
            ExampleTestFile
        ),

    ?assertEqual(
        "At line 22, there are too many (~p) blank lines; prefer respecting the configured limit (limit: 2).",
        Message
    ),
    ?assertEqual(22, LineNum),
    ?assertEqual(-1, ColumnNum).

wihout_limit(_Config) ->
    Rule = no_if_expression,
    RuleConfig = #{},
    ExampleTestFile = "elvis_results_new_item.erl",

    [#{message := Message, line_num := LineNum, column_num := ColumnNum}] =
        elvis_test_utils:elvis_core_apply_rule(
            _TestcaseConfig = [],
            elvis_style,
            Rule,
            RuleConfig,
            ExampleTestFile
        ),

    ?assertEqual("At line 25, column 5, an unexpected 'if' expression was found.", Message),
    ?assertEqual(25, LineNum),
    ?assertEqual(5, ColumnNum).

without_data(_Config) ->
    Rule = state_record_and_type,
    RuleConfig = #{},
    ExampleTestFile = "elvis_results_new_item.erl",

    [#{message := Message, line_num := LineNum, column_num := ColumnNum}] =
        elvis_test_utils:elvis_core_apply_rule(
            _TestcaseConfig = [],
            elvis_style,
            Rule,
            RuleConfig,
            ExampleTestFile
        ),

    ?assertEqual(
        "This module implements an OTP behavior but is missing a '#state{}' record.", Message
    ),
    ?assertEqual(-1, LineNum),
    ?assertEqual(-1, ColumnNum).

with_attrs_for_data(_Config) ->
    Rule = no_spec_with_records,
    RuleConfig = #{},
    ExampleTestFile = "elvis_results_new_item.erl",

    [#{message := Message, line_num := LineNum, column_num := ColumnNum}] =
        elvis_test_utils:elvis_core_apply_rule(
            _TestcaseConfig = [],
            elvis_style,
            Rule,
            RuleConfig,
            ExampleTestFile
        ),

    ?assertEqual(
        "At line 35, column 2, an unexpected record was found in a spec; prefer creating a type for it and using that.",
        Message
    ),
    ?assertEqual(35, LineNum),
    ?assertEqual(2, ColumnNum).

complete(_Config) ->
    Rule = max_anonymous_function_arity,
    RuleConfig = #{max_arity => 1},
    ExampleTestFile = "elvis_results_new_item.erl",

    [#{message := Message, line_num := LineNum, column_num := ColumnNum, info := [2]}] =
        elvis_test_utils:elvis_core_apply_rule(
            _TestcaseConfig = [],
            elvis_style,
            Rule,
            RuleConfig,
            ExampleTestFile
        ),

    ?assertEqual(
        "At line 46, column 9, the arity (~p) of the anonymous function is higher than the configured limit (limit: 1).",
        Message
    ),
    ?assertEqual(46, LineNum),
    ?assertEqual(9, ColumnNum).
