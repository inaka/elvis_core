-module(elvis_style).

-export([
         default/1,
         function_naming_convention/3,
         variable_naming_convention/3,
         macro_names/3,
         macro_module_names/3,
         operator_spaces/3,
         nesting_level/3,
         god_modules/3,
         no_if_expression/3,
         invalid_dynamic_call/3,
         used_ignored_variable/3,
         no_behavior_info/3,
         module_naming_convention/3,
         state_record_and_type/3,
         no_spec_with_records/3,
         dont_repeat_yourself/3,
         max_module_length/3,
         max_function_length/3,
         no_call/3,
         no_debug_call/3,
         no_common_caveats_call/3,
         no_nested_try_catch/3,
         atom_naming_convention/3,
         option/3
        ]).

-export_type([empty_rule_config/0]).
-export_type([ignorable/0]).

-define(INVALID_MACRO_NAME_REGEX_MSG,
        "The macro named ~p on line ~p does not respect the format "
        "defined by the regular expression '~p'.").

-define(MACRO_AS_MODULE_NAME_MSG,
        "Don't use macros (like ~s on line ~p) as module names.").
-define(MACRO_MODULE_NAMES_EXCEPTIONS,
        ["MODULE"]).

-define(MACRO_AS_FUNCTION_NAME_MSG,
            "Don't use macros (like ~s on line ~p) as function names.").

-define(OPERATOR_SPACE_MSG, "Missing space ~s ~p on line ~p").

-define(NESTING_LEVEL_MSG,
        "The expression on line ~p and column ~p is nested "
        "beyond the maximum level of ~p.").

-define(GOD_MODULES_MSG,
        "This module has too many functions (~p). "
        "Consider breaking it into a number of modules.").

-define(NO_IF_EXPRESSION_MSG,
        "Replace the 'if' expression on line ~p with a 'case' "
        "expression or function clauses.").

-define (INVALID_DYNAMIC_CALL_MSG,
         "Remove the dynamic function call on line ~p. "
         "Only modules that define callbacks should make dynamic calls.").

-define(USED_IGNORED_VAR_MSG,
        "Ignored variable is being used on line ~p and "
        "column ~p.").

-define(NO_BEHAVIOR_INFO,
        "Use the '-callback' attribute instead of 'behavior_info/1' "
        "on line ~p.").

-define(FUNCTION_NAMING_CONVENTION_MSG,
        "The function ~p does not respect the format defined by the "
        "regular expression '~p'.").

-define(VARIABLE_NAMING_CONVENTION_MSG,
        "The variable ~p on line ~p does not respect the format "
        "defined by the regular expression '~p'.").

-define(MODULE_NAMING_CONVENTION_MSG,
        "The module ~p does not respect the format defined by the "
        "regular expression '~p'.").

-define(STATE_RECORD_MISSING_MSG,
        "This module implements an OTP behavior but is missing "
        "a 'state' record.").

-define(STATE_TYPE_MISSING_MSG,
        "This module implements an OTP behavior and has a 'state' record "
        "but is missing a 'state()' type.").

-define(NO_SPEC_WITH_RECORDS,
        "The spec in line ~p uses a record, please define a type for the "
        "record and use that instead.").

-define(DONT_REPEAT_YOURSELF,
        "The code in the following (LINE, COL) locations has "
        "the same structure: ~s.").

-define(MAX_MODULE_LENGTH,
        "The code for module ~p has ~p lines which exceeds the "
        "maximum of ~p.").

-define(MAX_FUNCTION_LENGTH,
        "The code for function ~p/~w has ~p lines which exceeds the "
        "maximum of ~p.").

-define(NO_CALL_MSG,
        "The call to ~p:~p/~p on line ~p is in the no_call list.").

-define(NO_DEBUG_CALL_MSG,
        "Remove the debug call to ~p:~p/~p on line ~p.").

-define(NO_COMMON_CAVEATS_CALL_MSG,
        "The call to ~p:~p/~p on line ~p is in the list of "
        "Erlang Efficiency Guide common caveats.").

-define(NO_NESTED_TRY_CATCH,
        "Nested try...catch block starting at line ~p.").

-define(ATOM_NAMING_CONVENTION_MSG,
        "Atom ~p on line ~p does not respect the format "
        "defined by the regular expression '~p'.").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(Rule :: atom()) -> DefaultRuleConfig :: term().
default(macro_names) ->
    #{ regex => "^([A-Z][A-Z_0-9]+)$"
     };

default(macro_module_names) ->
    #{};

default(operator_spaces) ->
    #{ rules => [ {right, ","}
                , {right, "++"}
                , {left, "++"}
                ]
     };

default(nesting_level) ->
    #{ level => 4
     };

default(god_modules) ->
    #{ limit => 25
     };

default(no_if_expression) ->
    #{};

default(no_nested_try_catch) ->
    #{};

default(invalid_dynamic_call) ->
    #{};

default(used_ignored_variable) ->
    #{};

default(no_behavior_info) ->
    #{};

default(function_naming_convention) ->
    #{ regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$"
     };

default(variable_naming_convention) ->
    #{ regex => "^_?([A-Z][0-9a-zA-Z]*)$"
     };

default(module_naming_convention) ->
    #{ regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$"
     };

default(state_record_and_type) ->
    #{};

default(no_spec_with_records) ->
    #{};

default(dont_repeat_yourself) ->
    #{ min_complexity => 10
     };

default(max_module_length) ->
    #{ max_length => 500
     , count_comments => false
     , count_whitespace => false
     };

default(max_function_length) ->
    #{ max_length => 30
     , count_comments => false
     , count_whitespace => false
     };

default(no_call) ->
    #{ no_call_functions => []
     };

default(no_debug_call) ->
    #{ debug_functions => [ {ct, pal}
                          , {ct, print}
                          , {io, format, 1}
                          , {io, format, 2}
                          ]
     };

default(no_common_caveats_call) ->
    #{ caveat_functions => [ {timer, send_after, 2}
                           , {timer, send_after, 3}
                           , {timer, send_interval, 2}
                           , {timer, send_interval, 3}
                           , {erlang, size, 1}
                           ]
     };

default(atom_naming_convention) ->
    #{ regex => "^([a-z][a-z0-9]*_?)*(_SUITE)?$"
     , enclosed_atoms => ".*"
     }.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type empty_rule_config() :: #{ ignore => [ignorable()]
                              }.
-type ignorable() :: module() | {module(), atom()} | {module(), atom(), arity()}.

-type max_function_length_config() :: #{ ignore => [ignorable()]
                                       , max_length => non_neg_integer()
                                       , count_comments => boolean()
                                       , count_whitespace => boolean()
                                       }.

-type max_module_length_config() :: #{ ignore => [ignorable()]
                                     , count_comments => boolean()
                                     , count_whitespace => boolean()
                                     , max_length => integer()
                                     }.

-type function_naming_convention_config() :: #{ ignore => [ignorable()]
                                              , regex => string()
                                              }.

-spec function_naming_convention(elvis_config:config(),
                                 elvis_file:file(),
                                 function_naming_convention_config()) ->
    [elvis_result:item()].
function_naming_convention(Config, Target, RuleConfig) ->
    Regex = option(regex, RuleConfig, function_naming_convention),
    Root = get_root(Config, Target, RuleConfig),
    FunctionNames0 = elvis_code:function_names(Root),
    errors_for_function_names(Regex, FunctionNames0).

errors_for_function_names(_Regex, []) -> [];
errors_for_function_names(Regex, [FunctionName | RemainingFuncNames]) ->
    FunctionNameStr = unicode:characters_to_list(atom_to_list(FunctionName), unicode),
    case re:run(FunctionNameStr, Regex, [unicode]) of
        nomatch ->
            Msg = ?FUNCTION_NAMING_CONVENTION_MSG,
            Info = [FunctionNameStr, Regex],
            Result = elvis_result:new(item, Msg, Info, 1),
            [Result | errors_for_function_names(Regex, RemainingFuncNames)];
        {match, _} -> errors_for_function_names(Regex, RemainingFuncNames)
    end.

-type variable_naming_convention_config() :: #{ ignore => [ignorable()]
                                              , regex => string()
                                              }.
-spec variable_naming_convention(elvis_config:config(),
                                 elvis_file:file(),
                                 variable_naming_convention_config()) ->
    [elvis_result:item()].
variable_naming_convention(Config, Target, RuleConfig) ->
    Regex = option(regex, RuleConfig, variable_naming_convention),
    Root = get_root(Config, Target, RuleConfig),
    Vars =
        elvis_code:find(
            fun is_var/1, Root, #{traverse => all, mode => zipper}),
    check_variables_name(Regex, Vars).

-type macro_names_config() :: #{ ignore => [ignorable()]
                               , regex => string()
                               }.

-spec macro_names(elvis_config:config(),
                  elvis_file:file(),
                  macro_names_config()) ->
    [elvis_result:item()].
macro_names(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Regexp = option(regex, RuleConfig, macro_names),
    MacroNodes = elvis_code:find(fun is_macro_define_node/1, Root,
                                 #{traverse => all, mode => node}),
    check_macro_names(Regexp, MacroNodes, _ResultsIn = []).

-spec macro_module_names(elvis_config:config(),
                         elvis_file:file(),
                         empty_rule_config()) ->
    [elvis_result:item()].
macro_module_names(Config, Target, RuleConfig) ->
    {Src, _} = elvis_file:src(Target),
    Root = get_root(Config, Target, RuleConfig),
    elvis_utils:check_lines(Src, fun check_macro_module_names/3, [Root]).

-type operator_spaces_config() :: #{ ignore => [ignorable()]
                                   , rules => [{right | left, string()}]
                                   }.
-define(PUNCTUATION_SYMBOLS, [',', ';', 'dot', '->', ':', '::', '|', '||']).

-spec operator_spaces(elvis_config:config(),
                      elvis_file:file(),
                      operator_spaces_config()) ->
    [elvis_result:item()].
operator_spaces(Config, Target, RuleConfig) ->
    Rules = option(rules, RuleConfig, operator_spaces),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Root = get_root(Config, Target, RuleConfig),

    Zipper = elvis_code:code_zipper(Root),
    OpNodes = zipper:filter(fun is_operator_node/1, Zipper),

    Tokens = ktn_code:attr(tokens, Root),
    PunctuationTokens = lists:filter(fun is_punctuation_token/1, Tokens),

    Lines = elvis_utils:split_all_lines(Src),
    AllNodes = OpNodes ++ PunctuationTokens,

    FlatMap = fun(Rule) ->
                  check_operator_spaces(Lines, AllNodes, Rule, Encoding)
              end,
    lists:flatmap(FlatMap, Rules).

%% @doc Returns true when the node is an operator with more than one operand
-spec is_operator_node(ktn_code:tree_node()) -> boolean().
is_operator_node(Node) ->
    ktn_code:type(Node) =:= op andalso length(ktn_code:content(Node)) > 1.

%% @doc Returns true when the token is one of the ?PUNCTUATION_SYMBOLS
-spec is_punctuation_token(ktn_code:tree_node()) -> boolean().
is_punctuation_token(Node) ->
    Type = ktn_code:type(Node),
    lists:member(Type, ?PUNCTUATION_SYMBOLS).

-type nesting_level_config() :: #{ ignore => [ignorable()]
                                 , level => integer()
                                 }.

-spec nesting_level(elvis_config:config(),
                    elvis_file:file(),
                    nesting_level_config()) ->
    [elvis_result:item()].
nesting_level(Config, Target, RuleConfig) ->
    Level = option(level, RuleConfig, nesting_level),

    Root = get_root(Config, Target, RuleConfig),

    elvis_utils:check_nodes(Root, fun check_nesting_level/2, [Level]).

-type god_modules_config() :: #{ ignore => [ignorable()]
                               , limit => integer()
                               }.

-spec god_modules(elvis_config:config(),
                  elvis_file:file(),
                  god_modules_config()) ->
    [elvis_result:item()].
god_modules(Config, Target, RuleConfig) ->
    Limit = option(limit, RuleConfig, god_modules),

    Root = get_root(Config, Target, RuleConfig),

    Exported = elvis_code:exported_functions(Root),
    case length(Exported) of
        Count when Count > Limit ->
            Msg = ?GOD_MODULES_MSG,
            Result = elvis_result:new(item, Msg, [Count], 1),
            [Result];
        _ ->
            []
    end.

-spec no_if_expression(elvis_config:config(),
                       elvis_file:file(),
                       empty_rule_config()) ->
    [elvis_result:item()].
no_if_expression(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Predicate = fun(Node) -> ktn_code:type(Node) == 'if' end,
    ResultFun = result_node_line_fun(?NO_IF_EXPRESSION_MSG),
    case elvis_code:find(Predicate, Root) of
        [] ->
            [];
        IfExprs ->
            lists:map(ResultFun, IfExprs)
    end.

-spec invalid_dynamic_call(elvis_config:config(),
                           elvis_file:file(),
                           empty_rule_config()) ->
    [elvis_result:item()].
invalid_dynamic_call(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),

    Predicate = fun(Node) -> ktn_code:type(Node) == 'callback' end,
    case elvis_code:find(Predicate, Root) of
        [] ->
            check_invalid_dynamic_calls(Root);
        _Callbacks ->
            []
    end.

-spec used_ignored_variable(elvis_config:config(),
                            elvis_file:file(),
                            empty_rule_config()) ->
    [elvis_result:item()].
used_ignored_variable(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    ResultFun = result_node_line_col_fun(?USED_IGNORED_VAR_MSG),

    case elvis_code:find(fun is_ignored_var/1, Root, #{mode => zipper}) of
        [] ->
            [];
        UsedIgnoredVars ->
            lists:map(ResultFun, UsedIgnoredVars)
    end.

-spec no_behavior_info(elvis_config:config(),
                       elvis_file:file(),
                       empty_rule_config()) ->
    [elvis_result:item()].
no_behavior_info(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Children = ktn_code:content(Root),

    FilterFun =
        fun
            (Node) ->
                case ktn_code:type(Node) of
                    function ->
                        Name = ktn_code:attr(name, Node),
                        lists:member(Name,
                                     [behavior_info, behaviour_info]);
                    _ -> false
                end
        end,

    ResultFun = result_node_line_fun(?NO_BEHAVIOR_INFO),

    case lists:filter(FilterFun, Children) of
        [] ->
            [];
        BehaviorInfos ->
            lists:map(ResultFun, BehaviorInfos)
    end.

-type module_naming_convention_config() :: #{ ignore => [ignorable()]
                                            , regex => string()
                                            }.

-spec module_naming_convention(elvis_config:config(),
                               elvis_file:file(),
                               module_naming_convention_config()) ->
    [elvis_result:item()].
module_naming_convention(Config, Target, RuleConfig) ->
    Regex = option(regex, RuleConfig, module_naming_convention),
    IgnoreModules = option(ignore, RuleConfig, module_naming_convention),

    Root = get_root(Config, Target, RuleConfig),
    ModuleName = elvis_code:module_name(Root),

    case lists:member(ModuleName, IgnoreModules) of
        false ->
            ModuleNameStr = atom_to_list(ModuleName),
            case re:run(ModuleNameStr, Regex) of
                nomatch ->
                    Msg = ?MODULE_NAMING_CONVENTION_MSG,
                    Info = [ModuleNameStr, Regex],
                    Result = elvis_result:new(item, Msg, Info, 1),
                    [Result];
                {match, _} -> []
            end;
        true -> []
    end.

-spec state_record_and_type(elvis_config:config(),
                            elvis_file:file(),
                            empty_rule_config()) ->
    [elvis_result:item()].
state_record_and_type(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    case is_otp_module(Root) of
        true ->
            case {has_state_record(Root), has_state_type(Root)} of
                {true, true} -> [];
                {false, _} ->
                    Msg = ?STATE_RECORD_MISSING_MSG,
                    Result = elvis_result:new(item, Msg, [], 1),
                    [Result];
                {true, false} ->
                    Msg = ?STATE_TYPE_MISSING_MSG,
                    Result = elvis_result:new(item, Msg, [], 1),
                    [Result]
            end;
        false ->
            []
    end.

-spec no_spec_with_records(elvis_config:config(),
                           elvis_file:file(),
                           empty_rule_config()) ->
    [elvis_result:item()].
no_spec_with_records(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    case elvis_code:find(fun spec_includes_record/1, Root) of
        [] -> [];
        SpecNodes ->
            ResultFun = result_node_line_fun(?NO_SPEC_WITH_RECORDS),
            lists:map(ResultFun, SpecNodes)
    end.

-type dont_repeat_yourself_config() :: #{ ignore => [ignorable()]
                                        , min_complexity => non_neg_integer()
                                        }.

-spec dont_repeat_yourself(elvis_config:config(),
                           elvis_file:file(),
                           dont_repeat_yourself_config()) ->
    [elvis_result:item()].
dont_repeat_yourself(Config, Target, RuleConfig) ->
    MinComplexity = option(min_complexity, RuleConfig, dont_repeat_yourself),

    Root = get_root(Config, Target, RuleConfig),

    Nodes = find_repeated_nodes(Root, MinComplexity),

    LocationCat =
        fun
            ({Line, Col}, "") ->
                io_lib:format("(~p, ~p)", [Line, Col]);
            ({Line, Col}, Str) ->
                io_lib:format("~s, (~p, ~p)", [Str, Line, Col])
        end,
    ResultFun =
        fun([{Line, _} | _] = Locations) ->
                LocationsStr = lists:foldl(LocationCat, "", Locations),
                Info = [LocationsStr],
                Msg = ?DONT_REPEAT_YOURSELF,
                elvis_result:new(item, Msg, Info, Line)
        end,

    lists:map(ResultFun, Nodes).

-spec max_module_length(elvis_config:config(),
                        elvis_file:file(),
                        max_module_length_config()) ->
    [elvis_result:item()].
max_module_length(Config, Target, RuleConfig) ->
    MaxLength = option(max_length, RuleConfig, max_module_length),
    CountComments = option(count_comments, RuleConfig, max_module_length),
    CountWhitespace = option(count_whitespace, RuleConfig, max_module_length),

    Root = get_root(Config, Target, RuleConfig),
    {Src, _} = elvis_file:src(Target),


    ModuleName = elvis_code:module_name(Root),

    FilterFun =
        fun(Line) ->
                (CountComments orelse (not line_is_comment(Line)))
                    andalso (CountWhitespace
                             orelse (not line_is_whitespace(Line)))
        end,
    Lines = case elvis_utils:split_all_lines(Src, [trim]) of
                Ls when CountComments andalso CountWhitespace -> Ls;
                Ls -> lists:filter(FilterFun, Ls)
            end,

    case length(Lines) of
        L when L > MaxLength ->
            Info = [ModuleName, L, MaxLength],
            Msg = ?MAX_MODULE_LENGTH,
            Result = elvis_result:new(item, Msg, Info, 0),
            [Result];
        _ ->
            []
    end.

-spec max_function_length(elvis_config:config(),
                          elvis_file:file(),
                          max_function_length_config()) ->
    [elvis_result:item()].
max_function_length(Config, Target, RuleConfig) ->
    MaxLength = option(max_length, RuleConfig, max_function_length),
    CountComments = option(count_comments, RuleConfig, max_function_length),
    CountWhitespace = option(count_whitespace, RuleConfig, max_function_length),

    Root = get_root(Config, Target, RuleConfig),
    {Src, _} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    IsFunction = fun(Node) -> ktn_code:type(Node) == function end,
    Functions0 = elvis_code:find(IsFunction, Root),
    FilterFun =
        fun(Line) ->
                (CountComments orelse (not line_is_comment(Line)))
                    andalso (CountWhitespace
                             orelse (not line_is_whitespace(Line)))
        end,

    PairFun =
        fun(FunctionNode) ->
                Name = ktn_code:attr(name, FunctionNode),
                Arity = ktn_code:attr(arity, FunctionNode),
                {Min, Max} = node_line_limits(FunctionNode),
                FunLines = lists:sublist(Lines, Min, Max - Min + 1),
                FilteredLines = lists:filter(FilterFun, FunLines),
                L = length(FilteredLines),
                {Name, Arity, Min, L}
        end,
    FunLenInfos = lists:map(PairFun, Functions0),
    MaxLengthPred = fun({_, _, _, L}) -> L > MaxLength end,
    FunLenMaxPairs = lists:filter(MaxLengthPred, FunLenInfos),

    ResultFun =
        fun({Name, Arity, StartPos, L}) ->
                Info = [Name, Arity, L, MaxLength],
                Msg = ?MAX_FUNCTION_LENGTH,
                elvis_result:new(item, Msg, Info, StartPos)
        end,
    lists:map(ResultFun, FunLenMaxPairs).

-type function_spec() :: {module(), atom(), arity()}
                       | {module(), atom()}.

-type no_call_config() :: #{ ignore => [ignorable()]
                           , no_call_functions => [function_spec()]
                           }.
-spec no_call(elvis_config:config(),
              elvis_file:file(),
              no_call_config()) ->
    [elvis_result:item()].
no_call(Config, Target, RuleConfig) ->
    DefaultFns = option(no_call_functions, RuleConfig, no_call),
    no_call_common(Config, Target, DefaultFns, ?NO_CALL_MSG, RuleConfig).


-type no_debug_call_config() :: #{ ignore => [ignorable()]
                                 , debug_functions => [function_spec()]
                                 }.
-spec no_debug_call(elvis_config:config(),
                    elvis_file:file(),
                    no_debug_call_config()) ->
    [elvis_result:item()].
no_debug_call(Config, Target, RuleConfig) ->
    DefaultFns = option(debug_functions, RuleConfig, no_debug_call),
    no_call_common(Config, Target, DefaultFns, ?NO_DEBUG_CALL_MSG, RuleConfig).


-type no_common_caveats_call_config() :: #{ ignore => [ignorable()]
                                          , caveat_functions => [function_spec()]
                                          }.
-spec no_common_caveats_call(elvis_config:config(),
                             elvis_file:file(),
                             no_common_caveats_call_config()) ->
    [elvis_result:item()].

no_common_caveats_call(Config, Target, RuleConfig) ->
    DefaultFns = option(caveat_functions, RuleConfig, no_common_caveats_call),
    no_call_common(Config, Target, DefaultFns, ?NO_COMMON_CAVEATS_CALL_MSG, RuleConfig).

-spec node_line_limits(ktn_code:tree_node())->
    {Min :: integer(), Max :: integer()}.
node_line_limits(FunctionNode) ->
    Zipper = elvis_code:code_zipper(FunctionNode),
    LineFun = fun(N) -> {L, _} = ktn_code:attr(location, N), L end,
    % The first number in `lineNums' list is the location of the first
    % line of the function. That's why we use it for the `Min' value.
    LineNums = zipper:map(LineFun, Zipper),
    % Last function's line
    Max = lists:max(LineNums),
    % If you use `lists:min/1' here, you will get weird results when using
    % macros because most of the time macros are defined at the beginning of
    % the module, but your function's first line could be in the middle or
    % even at the end of the module.
    [Min | _] = LineNums, % Min = first function's line
    {Min, Max}.

-spec no_nested_try_catch(elvis_config:config(),
                          elvis_file:file(),
                          empty_rule_config()) ->
    [elvis_result:item()].
no_nested_try_catch(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Predicate = fun(Node) -> ktn_code:type(Node) == 'try' end,
    ResultFun = result_node_line_fun(?NO_NESTED_TRY_CATCH),
    case elvis_code:find(Predicate, Root) of
        [] -> [];
        TryExprs -> lists:flatmap(fun (TryExp) ->
                                      check_nested_try_catchs(ResultFun, TryExp)
                                  end, TryExprs)
    end.

-type atom_naming_convention_config() :: #{ ignore => [ignorable()]
                                          , regex => string()
                                          , enclosed_atoms => same | string()
                                          }.

-spec atom_naming_convention(elvis_config:config(),
                             elvis_file:file(),
                             atom_naming_convention_config()) ->
    [elvis_result:item()].
atom_naming_convention(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Regex = option(regex, RuleConfig, atom_naming_convention),
    RegexEnclosed
        = enclosed_atoms_regex_or_same(option(enclosed_atoms,
                                              RuleConfig,
                                              atom_naming_convention),
                                       Regex),
    AtomNodes = elvis_code:find(fun is_atom_node/1, Root, #{traverse => all, mode => node}),
    check_atom_names(Regex, RegexEnclosed, AtomNodes, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enclosed_atoms_regex_or_same(same, Regex) ->
    Regex;
enclosed_atoms_regex_or_same(RegexEnclosed, _Regex) ->
    RegexEnclosed.

check_atom_names(_Regex, _RegexEnclosed, [] = _AtomNodes, Acc) ->
    Acc;
check_atom_names(Regex, RegexEnclosed, [AtomNode | RemainingAtomNodes], AccIn) ->
    AtomName0 = ktn_code:attr(text, AtomNode),
    {IsEnclosed, AtomName} = string_strip_enclosed(AtomName0),
    RE = re_compile_for_atom_type(IsEnclosed, Regex, RegexEnclosed),
    AccOut
        = case re:run(_Subject = unicode:characters_to_list(AtomName, unicode), RE) of
              nomatch when not(IsEnclosed)->
                  Msg = ?ATOM_NAMING_CONVENTION_MSG,
                  {Line, _} = ktn_code:attr(location, AtomNode),
                  Info = [AtomName0, Line, Regex],
                  Result = elvis_result:new(item, Msg, Info),
                  AccIn ++ [Result];
              nomatch when IsEnclosed->
                  Msg = ?ATOM_NAMING_CONVENTION_MSG,
                  {Line, _} = ktn_code:attr(location, AtomNode),
                  Info = [AtomName0, Line, RegexEnclosed],
                  Result = elvis_result:new(item, Msg, Info),
                  AccIn ++ [Result];
              {match, _Captured} ->
                  AccIn
          end,
    check_atom_names(Regex, RegexEnclosed, RemainingAtomNodes, AccOut).

string_strip_enclosed([$' | Rest]) ->
  [$' | Reversed] = lists:reverse(Rest),
  IsEnclosed = true,
  EnclosedAtomName = lists:reverse(Reversed),
  {IsEnclosed, EnclosedAtomName};
string_strip_enclosed(NonEnclosedAtomName) ->
  IsEnclosed = false,
  {IsEnclosed, NonEnclosedAtomName}.

re_compile_for_atom_type(false = _IsEnclosed, Regex, _RegexEnclosed) ->
    {ok, RE} = re:compile(Regex, [unicode]),
    RE;
re_compile_for_atom_type(true = _IsEnclosed, _Regex, RegexEnclosed) ->
    {ok, RE} = re:compile(RegexEnclosed, [unicode]),
    RE.

is_atom_node(MaybeAtom) ->
    ktn_code:type(MaybeAtom) =:= atom.

%% Variables name
check_variables_name(_Regex, []) -> [];
check_variables_name(Regex, [Variable | RemainingVars]) ->
    VariableNameStr = atom_to_list(ktn_code:attr(name, Variable)),
    case re:run(VariableNameStr, Regex) of
        nomatch when VariableNameStr == "_" ->
            check_variables_name(Regex, RemainingVars);
        nomatch ->
            Msg = ?VARIABLE_NAMING_CONVENTION_MSG,
            {Line, _} = ktn_code:attr(location, Variable),
            Info = [VariableNameStr, Line, Regex],
            Result = elvis_result:new(item, Msg, Info, Line),
            [Result | check_variables_name(Regex, RemainingVars)];
        {match, _} -> check_variables_name(Regex, RemainingVars)
    end.

%% Result building

result_node_line_fun(Msg) ->
    fun(Node) ->
            {Line, _} = ktn_code:attr(location, Node),
            Info = [Line],
            elvis_result:new(item, Msg, Info, Line)
    end.

result_node_line_col_fun(Msg) ->
    fun(Node) ->
            {Line, Col} = ktn_code:attr(location, Node),
            Info = [Line, Col],
            elvis_result:new(item, Msg, Info, Line)
    end.

%%% Rule checking

%% Line Length

-spec line_is_comment(binary()) -> boolean().
line_is_comment(Line) ->
    case re:run(Line, "^[ \t]*%") of
        nomatch    -> false;
        {match, _} -> true
    end.

-spec line_is_whitespace(binary()) -> boolean().
line_is_whitespace(Line) ->
    case re:run(Line, "^[ \t]*$") of
        nomatch    -> false;
        {match, _} -> true
    end.

%% Macro Names

check_macro_names(_Regexp, [] = _MacroNodes, ResultsIn) ->
    ResultsIn;
check_macro_names(Regexp, [MacroNode | RemainingMacroNodes], ResultsIn) ->
    {ok, RE} = re:compile(Regexp, [unicode]),
    {MacroNameStripped0, MacroNameOriginal} = macro_name_from_node(MacroNode),
    MacroNameStripped = unicode:characters_to_list(MacroNameStripped0, unicode),
    ResultsOut
        = case re:run(_Subject = MacroNameStripped, RE) of
              nomatch ->
                  Msg = ?INVALID_MACRO_NAME_REGEX_MSG,
                  {Line, _} = ktn_code:attr(location, MacroNode),
                  Info = [MacroNameOriginal, Line, Regexp],
                  Result = elvis_result:new(item, Msg, Info),
                  ResultsIn ++ [Result];
              {match, _Captured} ->
                  ResultsIn
          end,
    check_macro_names(Regexp, RemainingMacroNodes, ResultsOut).

-dialyzer({no_match, is_macro_define_node/1}).
is_macro_define_node(MaybeMacro) ->
    case ktn_code:type(MaybeMacro) of
        {atom, [_, _], define} ->
            true;
        _ ->
            false
    end.

macro_name_from_node(MacroNode) ->
    MacroNodeValue = ktn_code:attr(value, MacroNode),
    MacroAsAtom = macro_as_atom(false, [var, atom, call], MacroNodeValue),
    MacroNameOriginal = atom_to_list(MacroAsAtom),
    MacroNameStripped = string:strip(MacroNameOriginal, both, $'),
    {MacroNameStripped, MacroNameOriginal}.

macro_as_atom({var, _Text, MacroAsAtom}, _Types, _MacroNodeValue) ->
    MacroAsAtom;
macro_as_atom({atom, _Text, MacroAsAtom}, _Types, _MacroNodeValue) ->
    MacroAsAtom;
macro_as_atom({call, _CallText, {Type, _AtomText, MacroAsAtom}, _VarArg}, _Types, _MacroNodeValue)
        when Type =:= var orelse Type =:= atom ->
    MacroAsAtom;
macro_as_atom(false, [Type | OtherTypes], MacroNodeValue) ->
    macro_as_atom(lists:keyfind(Type, _N = 1, MacroNodeValue), OtherTypes, MacroNodeValue).

%% Macro in Function Call as Module or Function Name

-spec check_macro_module_names(binary(), integer(), [term()]) ->
    no_result | {ok, elvis_result:item()}.
check_macro_module_names(Line, Num, [Root]) ->
    {ok, ModNameRegex} = re:compile("[?](\\w+)[:][?]?\\w+\\s*\\("),
    {ok, FunNameRegex} = re:compile("[?]?\\w+[:][?](\\w+)\\s*\\("),

    ModuleMsg = ?MACRO_AS_MODULE_NAME_MSG,
    ModuleResults =
        apply_macro_module_names(Line, Num, ModNameRegex, ModuleMsg, Root),

    FunctionMsg = ?MACRO_AS_FUNCTION_NAME_MSG,
    FunResults =
        apply_macro_module_names(Line, Num, FunNameRegex, FunctionMsg, Root),

    case FunResults ++ ModuleResults of
        [] ->
            no_result;
        Results ->
            {ok, Results}
    end.

-spec apply_macro_module_names(Line::binary(),
                               Num::integer(),
                               Regex::{re_pattern, _, _, _, _},
                               Msg::string(),
                               Root::term()) ->
    [elvis_result:item()].
apply_macro_module_names(Line, Num, Regex, Msg, Root) ->
    case re:run(Line, Regex, [{capture, all_but_first, index}]) of
        nomatch ->
            [];
        {match, [{Col, Len}]} ->
            MacroName = binary_to_list(binary:part(Line, Col, Len)),
            case
                lists:member(MacroName, ?MACRO_MODULE_NAMES_EXCEPTIONS)
                orelse not is_remote_call({Num, Col}, Root)
            of
                true ->
                    [];
                false ->
                    Result = elvis_result:new(item, Msg, [MacroName, Num], Num),
                    [Result]
            end
    end.

is_remote_call({Num, Col}, Root) ->
    case elvis_code:find_by_location(Root, {Num, Col}) of
        not_found ->
            true;
        {ok, Node0} ->
            Pred =
                fun(Zipper) ->
                        (Node0 == zipper:node(Zipper))
                            andalso has_remote_call_parent(Zipper)
                end,
            Opts = #{mode => zipper, traverse => all},
            [] =/= elvis_code:find(Pred, Root, Opts)
    end.

has_remote_call_parent(undefined) ->
    false;
has_remote_call_parent(Zipper) ->
    Node = zipper:node(Zipper),
    case ktn_code:type(Node) of
        remote ->
            true;
        call ->
            ktn_code:type(zipper:node(zipper:down(Zipper))) =:= remote;
        _ ->
            has_remote_call_parent(zipper:up(Zipper))
    end.

%% Operator Spaces
-spec check_operator_spaces(Lines :: [binary()],
                            OperatorNodes :: [ktn_code:tree_node()],
                            Rule :: {right | left, string()},
                            Encoding :: latin1 | utf8) ->
    [elvis_result:item()].
check_operator_spaces(Lines, OperatorNodes, {Position, Operator}, Encoding) ->
  FilterFun = fun(Node) -> ktn_code:attr(text, Node) =:= Operator end,
  Nodes = lists:filter(FilterFun, OperatorNodes),
  SpaceChar = $\s,
  FlatFun = fun(Node) ->
                Location = ktn_code:attr(location, Node),
                case
                  character_at_location(Position, Lines, Operator, Location, Encoding)
                of
                  SpaceChar -> [];
                  _         ->
                    Msg = ?OPERATOR_SPACE_MSG,
                    {Line, _Col} = Location,
                    Info = [Position, Operator, Line],
                    Result = elvis_result:new(item, Msg, Info, Line),
                    [Result]
                end
            end,
  lists:flatmap(FlatFun, Nodes).

-spec character_at_location(Position::atom(),
                            Lines::[binary()],
                            Operator::string(),
                            Location::{integer(), integer()},
                            Encoding::latin1|utf8) -> char().
character_at_location(Position, Lines, Operator, {LineNo, Col}, Encoding) ->
    Line = lists:nth(LineNo, Lines),
    OperatorLineStr = unicode:characters_to_list(Line, Encoding),
    ColToCheck = case Position of
        left  -> Col - 1;
        right -> Col + length(Operator)
    end,
    % If ColToCheck is greater than the length of OperatorLineStr variable, it
    % means the end of line was reached so return " " to make the check pass,
    % otherwise return the character at the given column.
    % NOTE: text below only applies when the given Position is equal to `right`,
    %       or Position is equal to `left` and Col is 1.
    SpaceChar = $\s,
    case ColToCheck =:= 0 orelse {Position, (ColToCheck > length(OperatorLineStr))} of
        true -> SpaceChar;
        {right, true}  -> SpaceChar;
        _ -> lists:nth(ColToCheck, OperatorLineStr)
    end.

%% Nesting Level
-spec check_nesting_level(ktn_code:tree_node(), [integer()]) ->
    [elvis_result:item()].
check_nesting_level(ParentNode, [MaxLevel]) ->
    case elvis_code:past_nesting_limit(ParentNode, MaxLevel) of
        [] -> [];
        NestedNodes ->
            Msg = ?NESTING_LEVEL_MSG,

            Fun = fun(Node) ->
                          {Line, Col} = ktn_code:attr(location, Node),
                          Info = [Line, Col, MaxLevel],
                          elvis_result:new(item, Msg, Info, Line)
                  end,

            lists:map(Fun, NestedNodes)
    end.

%% Invalid Dynamic Calls

-spec check_invalid_dynamic_calls(ktn_code:tree_node()) ->
    [elvis_result:item()].
check_invalid_dynamic_calls(Root) ->
    case elvis_code:find(fun is_dynamic_call/1, Root, #{ traverse => all }) of
        [] -> [];
        InvalidCalls ->
            ResultFun = result_node_line_fun(?INVALID_DYNAMIC_CALL_MSG),
            lists:map(ResultFun, InvalidCalls)
    end.

-spec is_dynamic_call(ktn_code:tree_node()) -> boolean().
is_dynamic_call(Node) ->
    case ktn_code:type(Node) of
        call ->
            FunctionSpec = ktn_code:node_attr(function, Node),
            case ktn_code:type(FunctionSpec) of
                remote ->
                    ModuleName = ktn_code:node_attr(module, FunctionSpec),
                    var == ktn_code:type(ModuleName);
                _Other ->
                    false
            end;
        _ ->
            false
    end.

%% Plain Variable
-spec is_var(zipper:zipper(_)) -> boolean().
is_var(Zipper) ->
    case ktn_code:type(zipper:node(Zipper)) of
        var ->
            PrevLocation =
                case ktn_code:attr(location, zipper:node(Zipper)) of
                    {L, 1} -> {L - 1, 9999};
                    {L, C} -> {L, C - 1}
                end,
            case elvis_code:find_token(zipper:root(Zipper), PrevLocation) of
                not_found -> true;
                {ok, PrevToken} -> ktn_code:type(PrevToken) /= '?'
            end;
        _NotVar -> false
    end.

%% Ignored Variable

-spec is_ignored_var(zipper:zipper(_)) ->
    boolean().
is_ignored_var(Zipper) ->
    Node = zipper:node(Zipper),
    case ktn_code:type(Node) of
        var ->
            Name = ktn_code:attr(name, Node),
            [FirstChar | _] = atom_to_list(Name),
            (FirstChar == $_)
                and (Name =/= '_')
                and not check_parent_match(Zipper);
        _OtherType -> false
    end.

check_parent_match(Zipper) ->
    case zipper:up(Zipper) of
        undefined -> false;
        ParentZipper ->
            Parent = zipper:node(ParentZipper),
            case ktn_code:type(Parent) of
                match ->
                    zipper:down(ParentZipper) == Zipper;
                _ -> check_parent_match(ParentZipper)
            end
    end.

%% State record in OTP module

-spec is_otp_module(ktn_code:tree_node()) -> boolean().
is_otp_module(Root) ->
    OtpSet = sets:from_list([gen_server,
                             gen_event,
                             gen_fsm,
                             gen_statem,
                             supervisor_bridge
                            ]),
    IsBehaviorAttr =
        fun(Node) ->
            behavior == ktn_code:type(Node) orelse
            behaviour == ktn_code:type(Node)
        end,
    case elvis_code:find(IsBehaviorAttr, Root) of
        [] ->
            false;
        Behaviors ->
            ValueFun = fun(Node) -> ktn_code:attr(value, Node) end,
            Names = lists:map(ValueFun, Behaviors),
            BehaviorsSet = sets:from_list(Names),
            case sets:to_list(sets:intersection(OtpSet, BehaviorsSet)) of
                [] -> false;
                _ -> true
            end
    end.

-spec has_state_record(ktn_code:tree_node()) -> boolean().
has_state_record(Root) ->
    IsStateRecord =
        fun(Node) ->
                (record_attr == ktn_code:type(Node))
                    and (state == ktn_code:attr(name, Node))
        end,
    case elvis_code:find(IsStateRecord, Root) of
        [] -> false;
        _ -> true
    end.

-spec has_state_type(ktn_code:tree_node()) -> boolean().
has_state_type(Root) ->
    IsStateType =
        fun(Node) ->
                (type_attr == ktn_code:type(Node))
                    and (state == ktn_code:attr(name, Node))
        end,
    elvis_code:find(IsStateType, Root) /= [].

%% Spec includes records

-spec spec_includes_record(ktn_code:tree_node()) -> boolean().
spec_includes_record(Node) ->
    IsTypeRecord = fun(Child) ->
                           (ktn_code:type(Child) == type)
                               and (ktn_code:attr(name, Child) == record)
                   end,
    Opts = #{traverse => all},
    (ktn_code:type(Node) == spec)
        and (elvis_code:find(IsTypeRecord, Node, Opts) /= []).

%% Don't repeat yourself

-spec find_repeated_nodes(ktn_code:tree_node(), non_neg_integer()) ->
    [ktn_code:tree_node()].
find_repeated_nodes(Root, MinComplexity) ->
    TypeAttrs = #{var    => [location, name, text],
                  clause => [location, text]},

    FoldFun =
        fun(Node, Map) ->
                Zipper = elvis_code:code_zipper(Node),
                case zipper:size(Zipper) of
                    Count when Count >= MinComplexity ->
                        Loc = ktn_code:attr(location, Node),
                        StrippedNode = remove_attrs_zipper(Zipper, TypeAttrs),

                        ValsSet = maps:get(StrippedNode, Map, sets:new()),
                        NewValsSet = sets:add_element(Loc, ValsSet),
                        maps:put(StrippedNode, NewValsSet, Map);
                    _ ->
                        Map
                end
        end,
    ZipperRoot = elvis_code:code_zipper(Root),
    Grouped = zipper:fold(FoldFun, #{}, ZipperRoot),

    Repeated = filter_repeated(Grouped),
    LocationSets = maps:values(Repeated),
    Locations = lists:map(fun sets:to_list/1, LocationSets),

    lists:map(fun lists:sort/1, Locations).

-spec remove_attrs_zipper(zipper:zipper(_), map()) -> ktn_code:tree_node().
remove_attrs_zipper(Zipper, TypeAttrs) ->
    zipper:fmap(fun remove_attrs/2, [TypeAttrs], Zipper).

-spec remove_attrs(ktn_code:tree_node() | [ktn_code:tree_node()], map()) ->
    ktn_code:tree_node().
remove_attrs(Nodes, TypeAttrs) when is_list(Nodes) ->
    [remove_attrs(Node, TypeAttrs) || Node <- Nodes];
remove_attrs(#{attrs := Attrs,
               type := Type,
               node_attrs := NodeAttrs} = Node,
             TypeAttrs) ->
    AttrsName = maps:get(Type, TypeAttrs, [location]),
    AttrsNoLoc = maps:without(AttrsName, Attrs),
    NodeAttrsNoLoc =
        [{ Key
         , remove_attrs_zipper(elvis_code:code_zipper(Value),
                               TypeAttrs)}
         || {Key, Value} <- maps:to_list(NodeAttrs)],

    Node#{attrs => AttrsNoLoc,
          node_attrs => maps:from_list(NodeAttrsNoLoc)};
remove_attrs(#{attrs := Attrs, type := Type} = Node, TypeAttrs) ->
    AttrsName = maps:get(Type, TypeAttrs, [location]),
    AttrsNoLoc = maps:without(AttrsName, Attrs),
    Node#{attrs => AttrsNoLoc};
remove_attrs(Node, _TypeAttrs) ->
    Node.

-spec filter_repeated(map()) -> map().
filter_repeated(NodesLocs) ->
    NotRepeated = [Node
                   || {Node, LocationSet} <- maps:to_list(NodesLocs),
                      sets:size(LocationSet) == 1],

    RepeatedMap = maps:without(NotRepeated, NodesLocs),

    RepeatedNodes = maps:keys(RepeatedMap),
    Nested = [Node
              || Node <- RepeatedNodes,
                 Parent <- RepeatedNodes,
                 Node =/= Parent,
                 is_children(Parent, Node)],

    maps:without(Nested, RepeatedMap).

is_children(Parent, Node) ->
    Zipper = elvis_code:code_zipper(Parent),
    [] =/= zipper:filter(fun(Child) -> Child == Node end, Zipper).

%% No call
-spec no_call_common(elvis_config:config(),
                     elvis_file:file(),
                     [function_spec()],
                     string(),
                     RuleConfig :: elvis_core:rule_config()
                    ) ->
    [elvis_result:item()].
no_call_common(Config, Target, NoCallFuns, Msg, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),

    IsCall = fun(Node) -> ktn_code:type(Node) =:= 'call' end,
    Calls = elvis_code:find(IsCall, Root),
    check_no_call(Calls, Msg, NoCallFuns).

-spec check_no_call([ktn_code:tree_node()], string(), [function_spec()]) ->
    [elvis_result:item()].
check_no_call(Calls, Msg, NoCallFuns) ->
    DebugCalls = [Call || Call <- Calls, is_in_call_list(Call, NoCallFuns)],
    ResultFun = fun(Call) ->
                        {M, F, A} = call_mfa(Call),
                        {Line, _} = ktn_code:attr(location, Call),
                        elvis_result:new(item,
                                         Msg,
                                         [M, F, A, Line],
                                         Line)
                end,
    lists:map(ResultFun, DebugCalls).

is_in_call_list(Call, DebugFuns) ->
    MFA = call_mfa(Call),
    MatchFun = fun(Spec) -> fun_spec_match(Spec, MFA) end,
    lists:any(MatchFun, DebugFuns).

call_mfa(Call) ->
    FunctionSpec = ktn_code:node_attr(function, Call),
    M = ktn_code:attr(value, ktn_code:node_attr(module, FunctionSpec)),
    F = ktn_code:attr(value, ktn_code:node_attr(function, FunctionSpec)),
    A = length(ktn_code:content(Call)),
    {M, F, A}.

fun_spec_match({M, F}, {M, F, _}) -> true;
fun_spec_match({M, F, A}, {M, F, A}) -> true;
fun_spec_match(_, _) -> false.

%% No nested try...catch blocks

check_nested_try_catchs(ResultFun, TryExp) ->
    Predicate = fun(Node) -> ktn_code:type(Node) == 'try' end,
    lists:filtermap(fun (Node) when Node /= TryExp ->
                             {true, ResultFun(Node)};
                        (_) ->
                             false
                    end,
                    elvis_code:find(Predicate, TryExp)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option(OptionName, RuleConfig, Rule) -> OptionValue
      when OptionName :: atom(),
           RuleConfig :: elvis_core:rule_config(),
           Rule :: atom(),
           OptionValue :: term().
option(OptionName, RuleConfig, Rule) ->
    maybe_default_option(maps:get(OptionName, RuleConfig, undefined), OptionName, Rule).

-spec maybe_default_option(UserDefinedOptionValue, OptionName, Rule) -> OptionValue
      when UserDefinedOptionValue :: undefined | term(),
           OptionName :: atom(),
           Rule :: atom(),
           OptionValue :: term().
maybe_default_option(undefined = _UserDefinedOptionValue, OptionName, Rule) ->
    maps:get(OptionName, default(Rule));
maybe_default_option(UserDefinedOptionValue, _OptionName, _Rule) ->
    UserDefinedOptionValue.

-spec get_root(Config, Target, RuleConfig) -> Res when
      Config :: elvis_config:config(),
      Target :: elvis_file:file(),
      RuleConfig :: Options :: #{ atom() => term() },
      Res :: ktn_code:tree_node().
get_root(Config, Target, RuleConfig) ->
    {Root0, File0} = elvis_file:parse_tree(Config, Target, RuleConfig),
    case maps:get(ruleset, Config, undefined) of
        beam_files ->
            maps:get(abstract_parse_tree, File0);
        _ ->
            Root0
    end.
