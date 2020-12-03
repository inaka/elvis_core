-module(elvis_rulesets).

-export([rules/1,
         set_rulesets/1]).

-spec set_rulesets(#{atom() => list()}) -> ok.
set_rulesets(Rulesets) ->
    Tid = ensure_clean_table(),
    lists:foreach(fun({Name, Rules}) ->
                          true = ets:insert(Tid, {Name, Rules})
                  end, maps:to_list(Rulesets)).

-spec rules(Group::atom()) -> [elvis_core:rule()].
rules(erl_files) ->
    lists:map(
        fun ({Mod, Rule}) ->
            {Mod, Rule, Mod:default(Rule)}
        end,
        code_rules()
    );
rules(beam_files) ->
    lists:map(
        fun (Rule) ->
            {elvis_style, Rule, elvis_style:default(Rule)}
        end,
        code_rules()
    );
rules(makefiles) ->
    lists:map(
        fun (Rule) ->
            {elvis_project, Rule, elvis_project:default(Rule)}
        end,
        [ no_deps_master_erlang_mk
        , protocol_for_deps_erlang_mk
        ]);
rules(rebar_config) ->
    lists:map(
        fun (Rule) ->
            {elvis_project, Rule, elvis_project:default(Rule)}
        end,
        [ no_deps_master_rebar
        , protocol_for_deps_rebar
        ]);
rules(elvis_config) ->
    lists:map(
        fun (Rule) ->
            {elvis_project, Rule, elvis_project:default(Rule)}
        end,
        [ old_configuration_format
        ]);
rules(Group) ->
    try ets:lookup_element(?MODULE, Group, 2)
    catch
        error:badarg -> []
    end.

ensure_clean_table() ->
    case ets:info(?MODULE) of
        undefined ->
            ets:new(?MODULE, [set, named_table, {keypos, 1}]);
        _ ->
            true = ets:delete_all_objects(?MODULE),
            ?MODULE
    end.


code_rules() -> [
    line_length
  , no_tabs
  , no_trailing_whitespace
  , macro_names
  , macro_module_names
  , operator_spaces
  , nesting_level
  , god_modules
  , no_if_expression
  , invalid_dynamic_call
  , used_ignored_variable
  , no_behavior_info
  , module_naming_convention
  , function_naming_convention
  , state_record_and_type
  , no_spec_with_records
  , dont_repeat_yourself
  , no_debug_call
  , variable_naming_convention
  , no_nested_try_catch
  , atom_naming_convention
].
