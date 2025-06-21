-module(elvis_style).
-behaviour(elvis_ruleset).

-export([
    default/1,
    function_naming_convention/1,
    variable_naming_convention/1,
    consistent_variable_casing/1,
    macro_names/1,
    no_macros/1,
    no_specs/1,
    no_types/1,
    no_nested_hrls/1,
    no_block_expressions/1,
    operator_spaces/1,
    no_space/1,
    no_space_after_pound/1,
    nesting_level/1,
    god_modules/1,
    no_if_expression/1,
    invalid_dynamic_call/1,
    used_ignored_variable/1,
    no_behavior_info/1,
    module_naming_convention/1,
    state_record_and_type/1,
    no_spec_with_records/1,
    dont_repeat_yourself/1,
    max_module_length/1,
    max_anonymous_function_arity/1,
    max_function_arity/1,
    max_function_length/1,
    max_function_clause_length/1,
    no_call/1,
    no_debug_call/1,
    no_common_caveats_call/1,
    no_nested_try_catch/1,
    no_successive_maps/1,
    atom_naming_convention/1,
    no_throw/1,
    no_dollar_space/1,
    no_author/1,
    no_import/1,
    no_catch_expressions/1,
    no_single_clause_case/1,
    no_single_match_maybe/1,
    numeric_format/1,
    behaviour_spelling/1,
    always_shortcircuit/1,
    consistent_generic_type/1,
    export_used_types/1,
    no_match_in_condition/1,
    param_pattern_matching/1,
    private_data_types/1,
    option/3,
    no_init_lists/1,
    ms_transform_included/1,
    no_boolean_in_comparison/1,
    no_operation_on_same_value/1,
    no_receive_without_timeout/1,
    root/1
]).

-export_type([ignorable/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(RuleName :: atom()) -> elvis_core:rule_config().
default(no_init_lists) ->
    #{
        behaviours =>
            [gen_server, gen_statem, gen_fsm, supervisor, supervisor_bridge, gen_event]
    };
default(macro_names) ->
    #{regex => "^[A-Z](_?[A-Z0-9]+)*$"};
default(operator_spaces) ->
    #{
        rules =>
            [
                {right, "++"},
                {left, "++"},
                {right, "="},
                {left, "="},
                {right, "+"},
                {left, "+"},
                {right, "-"},
                {left, "-"},
                {right, "*"},
                {left, "*"},
                {right, "/"},
                {left, "/"},
                {right, "=<"},
                {left, "=<"},
                {right, "<"},
                {left, "<"},
                {right, ">"},
                {left, ">"},
                {right, ">="},
                {left, ">="},
                {right, "=="},
                {left, "=="},
                {right, "=:="},
                {left, "=:="},
                {right, "/="},
                {left, "/="},
                {right, "=/="},
                {left, "=/="},
                {right, "--"},
                {left, "--"},
                {right, "=>"},
                {left, "=>"},
                {right, ":="},
                {left, ":="},
                {right, "<-"},
                {left, "<-"},
                {right, "<="},
                {left, "<="},
                {right, "||"},
                {left, "||"},
                {right, "|"},
                {left, "|"},
                {right, "::"},
                {left, "::"},
                {right, "->"},
                {left, "->"},
                {right, ","},
                {right, "!"},
                {left, "!"},
                {right, "?="},
                {left, "?="},
                {right, ";"},
                {right, "<:="},
                {left, "<:="},
                {right, "<:-"},
                {left, "<:-"},
                {right, "&&"},
                {left, "&&"}
            ]
    };
default(no_space) ->
    % ) one can happen at the start of lines; all others can't
    #{
        rules =>
            [
                {right, "("},
                {left, ")"},
                {left, ","},
                {left, ":"},
                {right, ":"},
                {right, "#"},
                {right, "?"},
                {left, "."},
                {left, ";"}
            ]
    };
default(nesting_level) ->
    #{level => 4};
default(god_modules) ->
    #{limit => 25};
default(function_naming_convention) ->
    #{regex => "^[a-z](_?[a-z0-9]+)*(_test_)?$", forbidden_regex => undefined};
default(variable_naming_convention) ->
    #{regex => "^_?([A-Z][0-9a-zA-Z]*)$", forbidden_regex => undefined};
default(module_naming_convention) ->
    #{regex => "^[a-z](_?[a-z0-9]+)*(_SUITE)?$", forbidden_regex => undefined};
default(dont_repeat_yourself) ->
    #{min_complexity => 10};
default(max_module_length) ->
    #{
        max_length => 500,
        count_comments => false,
        count_whitespace => false,
        count_docs => false
    };
default(max_anonymous_function_arity) ->
    #{max_arity => 5};
default(max_function_arity) ->
    #{max_arity => 8, non_exported_max_arity => 10};
default(max_function_length) ->
    #{
        max_length => 30,
        count_comments => false,
        count_whitespace => false
    };
default(max_function_clause_length) ->
    #{
        max_length => 30,
        count_comments => false,
        count_whitespace => false
    };
default(no_call) ->
    #{no_call_functions => []};
default(no_debug_call) ->
    #{
        debug_functions =>
            [
                {ct, pal},
                {ct, print},
                {erlang, display, 1},
                {io, format, 1},
                {io, format, 2},
                {io, put_chars, 1},
                {io, put_chars, 2},
                {dbg, '_'},
                {dyntrace, '_'},
                {instrument, '_'}
            ]
    };
default(no_common_caveats_call) ->
    #{
        caveat_functions =>
            [
                {timer, send_after, 2},
                {timer, send_after, 3},
                {timer, send_interval, 2},
                {timer, send_interval, 3},
                {erlang, size, 1},
                {gen_statem, call, 2},
                {gen_server, call, 2},
                {gen_event, call, 3},
                {erlang, list_to_atom, 1},
                {erlang, binary_to_atom, 1},
                {erlang, binary_to_atom, 2}
            ]
    };
default(atom_naming_convention) ->
    #{
        regex => "^[a-z](_?[a-z0-9]+)*(_SUITE)?$",
        enclosed_atoms => ".*",
        forbidden_regex => undefined,
        forbidden_enclosed_regex => undefined
    };
%% Not restrictive. Those who want more restrictions can set it like "^[^_]*$"
default(numeric_format) ->
    #{
        regex => ".*",
        int_regex => same,
        float_regex => same
    };
default(behaviour_spelling) ->
    #{spelling => behaviour};
default(param_pattern_matching) ->
    #{side => right};
default(consistent_generic_type) ->
    #{preferred_type => term};
default(private_data_types) ->
    #{apply_to => [record]};
default(no_operation_on_same_value) ->
    #{
        operations => [
            'and',
            'or',
            'xor',
            '==',
            '/=',
            '=<',
            '<',
            '>=',
            '>',
            '=:=',
            '=/=',
            'andalso',
            'orelse',
            '=',
            '--'
        ]
    };
default(no_macros) ->
    #{
        allow => []
    };
default(RuleWithEmptyDefault) when
    RuleWithEmptyDefault =:= no_specs;
    RuleWithEmptyDefault =:= no_types;
    RuleWithEmptyDefault =:= no_nested_hrls;
    RuleWithEmptyDefault =:= no_block_expressions;
    RuleWithEmptyDefault =:= no_if_expression;
    RuleWithEmptyDefault =:= no_nested_try_catch;
    RuleWithEmptyDefault =:= no_successive_maps;
    RuleWithEmptyDefault =:= invalid_dynamic_call;
    RuleWithEmptyDefault =:= used_ignored_variable;
    RuleWithEmptyDefault =:= no_behavior_info;
    RuleWithEmptyDefault =:= state_record_and_type;
    RuleWithEmptyDefault =:= no_spec_with_records;
    RuleWithEmptyDefault =:= no_throw;
    RuleWithEmptyDefault =:= no_dollar_space;
    RuleWithEmptyDefault =:= no_author;
    RuleWithEmptyDefault =:= no_import;
    RuleWithEmptyDefault =:= no_catch_expressions;
    RuleWithEmptyDefault =:= no_single_clause_case;
    RuleWithEmptyDefault =:= no_single_match_maybe;
    RuleWithEmptyDefault =:= no_match_in_condition;
    RuleWithEmptyDefault =:= always_shortcircuit;
    RuleWithEmptyDefault =:= no_space_after_pound;
    RuleWithEmptyDefault =:= export_used_types;
    RuleWithEmptyDefault =:= consistent_variable_casing;
    RuleWithEmptyDefault =:= ms_transform_included;
    RuleWithEmptyDefault =:= no_boolean_in_comparison;
    RuleWithEmptyDefault =:= no_receive_without_timeout
->
    #{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type ignorable() :: module() | {module(), atom()} | {module(), atom(), arity()}.

-type binary_part() :: {Start :: non_neg_integer(), Length :: integer()}.

function_naming_convention(RuleCfg) ->
    Regex = option(regex, RuleCfg, function_naming_convention),
    ForbiddenRegex = option(forbidden_regex, RuleCfg, function_naming_convention),
    Root = root(RuleCfg),
    Functions = elvis_code:find_by_types([function], Root),
    errors_for_function_names(Regex, ForbiddenRegex, Functions).

errors_for_function_names(_Regex, _ForbiddenRegex, []) ->
    [];
errors_for_function_names(Regex, ForbiddenRegex, [Function | RemainingFunctions]) ->
    FunctionName = ktn_code:attr(name, Function),
    FunctionNameStr = unicode:characters_to_list(atom_to_list(FunctionName)),
    case re:run(FunctionNameStr, Regex, [unicode]) of
        nomatch ->
            [
                elvis_result:new_item(
                    "the name of function '~p' is not acceptable by regular expression '~p'",
                    [FunctionNameStr, Regex],
                    #{node => Function}
                )
                | errors_for_function_names(Regex, ForbiddenRegex, RemainingFunctions)
            ];
        {match, _} ->
            case ForbiddenRegex of
                undefined ->
                    errors_for_function_names(Regex, ForbiddenRegex, RemainingFunctions);
                ForbiddenRegex ->
                    case re:run(FunctionNameStr, ForbiddenRegex, [unicode]) of
                        {match, _} ->
                            [
                                elvis_result:new_item(
                                    "the name of function '~p' is forbidden by "
                                    "regular expression '~p'",
                                    [FunctionNameStr, ForbiddenRegex],
                                    #{node => Function}
                                )
                                | errors_for_function_names(
                                    Regex, ForbiddenRegex, RemainingFunctions
                                )
                            ];
                        nomatch ->
                            errors_for_function_names(Regex, ForbiddenRegex, RemainingFunctions)
                    end
            end
    end.

%% @todo Use maps:groups_from_list/2 when we specify OTP25 as the minimum OTP version
consistent_variable_casing(RuleCfg) ->
    Root = root(RuleCfg),
    Vars = elvis_code:find(fun is_var/1, Root, #{traverse => all, filtered_from => zipper}),
    Grouped =
        maps:to_list(
            lists:foldr(
                fun(Var0, Acc) ->
                    Var = zipper:node(Var0),
                    VarName = canonical_variable_name(Var),
                    maps:update_with(
                        string:uppercase(VarName),
                        fun(Locations) -> [#{name => VarName, var => Var} | Locations] end,
                        [#{name => VarName, var => Var}],
                        Acc
                    )
                end,
                #{},
                Vars
            )
        ),
    lists:flatmap(fun check_variable_casing_consistency/1, Grouped).

canonical_variable_name(Var) ->
    case atom_to_list(ktn_code:attr(name, Var)) of
        [$_ | Rest] ->
            Rest;
        VarNameStr ->
            VarNameStr
    end.

check_variable_casing_consistency({_, [#{name := FirstName, var := FirstVar} | Others]}) ->
    case lists:usort([OtherName || #{name := OtherName} <- Others, OtherName =/= FirstName]) of
        [] ->
            [];
        OtherNames ->
            [
                elvis_result:new_item(
                    "variable '~p' (first used in line ~p) is written in "
                    "different ways within the module: ~p",
                    [FirstName, line(FirstVar), OtherNames],
                    #{node => FirstVar}
                )
            ]
    end.

variable_naming_convention(RuleCfg) ->
    Regex = option(regex, RuleCfg, variable_naming_convention),
    ForbiddenRegex = option(forbidden_regex, RuleCfg, variable_naming_convention),
    Root = root(RuleCfg),
    Vars = elvis_code:find(fun is_var/1, Root, #{traverse => all, filtered_from => zipper}),
    check_variables_name(Regex, ForbiddenRegex, Vars).

macro_names(RuleCfg) ->
    Regexp = option(regex, RuleCfg, macro_names),
    RE = re_compile(Regexp, [unicode]),

    MacroNodes = elvis_code:find(
        #{
            of_types => [define],
            inside => root(RuleCfg),
            filtered_by => fun(MacroNode) ->
                re:run(macro_name_from(MacroNode, stripped), RE) =:= nomatch
            end,
            traverse => all
        }
    ),

    [
        elvis_result:new_item(
            "the name of macro '~p' is not acceptable by "
            "regular expression '~p'",
            [macro_name_from(MacroNode, original), Regexp],
            #{node => MacroNode}
        )
     || MacroNode <- MacroNodes
    ].

no_macros(RuleCfg) ->
    AllowedMacros = option(allow, RuleCfg, no_macros) ++ eep_predef_macros() ++ logger_macros(),

    MacroNodes = elvis_code:find(
        #{
            of_types => [macro],
            inside => root(RuleCfg),
            filtered_by => fun(MacroNode) ->
                Macro = list_to_atom(ktn_code:attr(name, MacroNode)),
                not lists:member(Macro, AllowedMacros)
            end
        }
    ),

    [
        elvis_result:new_item(
            "an avoidable macro '~s' was found; prefer no macros",
            [ktn_code:attr(name, MacroNode)],
            #{node => MacroNode}
        )
     || MacroNode <- MacroNodes
    ].

no_types(RuleCfg) ->
    TypeAttrNodes = elvis_code:find(
        #{
            of_types => [type_attr],
            inside => root(RuleCfg)
        }
    ),

    [
        elvis_result:new_item(
            "unexpected `-type` attribute '~p' was found; "
            "avoid specifying types in .hrl files",
            [ktn_code:attr(name, TypeAttrNode)],
            #{node => TypeAttrNode}
        )
     || TypeAttrNode <- TypeAttrNodes
    ].

no_nested_hrls(RuleCfg) ->
    IncludeNodes = elvis_code:find(
        #{
            of_types => [include, include_lib],
            inside => root(RuleCfg)
        }
    ),

    [
        elvis_result:new_item(
            "unexpected nested '-include[_lib]' attribute ('~p') was found; "
            "avoid including .hrl files in .hrl files",
            [ktn_code:attr(value, IncludeNode)],
            #{node => IncludeNode}
        )
     || IncludeNode <- IncludeNodes
    ].

no_specs(RuleCfg) ->
    SpecNodes = elvis_code:find(
        #{
            of_types => [spec],
            inside => root(RuleCfg)
        }
    ),

    [
        elvis_result:new_item(
            "an unexpected spec for was found function '~p'; avoid specs in .hrl files",
            [ktn_code:attr(name, SpecNode)],
            #{node => SpecNode}
        )
     || SpecNode <- SpecNodes
    ].

no_block_expressions(RuleCfg) ->
    BlockExprs = elvis_code:find(
        #{
            of_types => ['begin'],
            inside => tokens_as_content(root(RuleCfg)),
            traverse => all
        }
    ),

    [
        elvis_result:new_item(
            "an avoidable block expression ('begin...end') was found",
            #{node => BlockExpr}
        )
     || BlockExpr <- BlockExprs
    ].

eep_predef_macros() ->
    % From unexported epp:predef_macros/1
    [
        'BASE_MODULE',
        'BASE_MODULE_STRING',
        'BEAM',
        'FEATURE_AVAILABLE',
        'FEATURE_ENABLED',
        'FILE',
        'FUNCTION_ARITY',
        'FUNCTION_NAME',
        'LINE',
        'MACHINE',
        'MODULE',
        'MODULE_STRING',
        'OTP_RELEASE'
    ].

logger_macros() ->
    % From logger.hrl
    [
        'LOG',
        'LOG_ALERT',
        'LOG_CRITICAL',
        'LOG_DEBUG',
        'LOG_EMERGENCY',
        'LOG_ERROR',
        'LOG_INFO',
        'LOG_NOTICE',
        'LOG_WARNING'
    ].

no_space_after_pound({_Config, Target, _RuleConfig} = RuleCfg) ->
    Root = root(RuleCfg),
    Tokens = ktn_code:attr(tokens, Root),
    TextNodes = lists:filter(fun is_text_node/1, Tokens),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src),
    check_spaces(Lines, TextNodes, {right, "#"}, Encoding, {should_not_have, []}).

punctuation_symbols() -> [',', ';', dot, '->', ':', '::', '|', '||'].

operator_spaces({_Config, Target, _RuleConfig} = RuleCfg) ->
    Rules = option(rules, RuleCfg, operator_spaces),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Root = root(RuleCfg),

    Zipper = elvis_code:code_zipper(Root),
    OpNodes = zipper:filter(fun is_operator_node/1, Zipper),

    OperatorsInTokens = ['=', '&&' | punctuation_symbols()],
    PunctuationTokens = elvis_code:find_by_types_in_tokens(OperatorsInTokens, Root),

    Lines = elvis_utils:split_all_lines(Src),
    AllNodes = OpNodes ++ PunctuationTokens,

    FlatMap =
        fun(Rule) -> check_spaces(Lines, AllNodes, Rule, Encoding, {should_have, []}) end,
    lists:flatmap(FlatMap, Rules).

%% @doc Returns true when the node is an operator with more than one operand
-spec is_operator_node(ktn_code:tree_node()) -> boolean().
is_operator_node(Node) ->
    NodeType = ktn_code:type(Node),
    OpOrMatch = [op | match_operators()],
    ExtraOpsTypes = [
        b_generate,
        b_generate_strict,
        generate,
        generate_strict,
        m_generate,
        m_generate_strict,
        map_field_assoc,
        map_field_exact
    ],
    (length(ktn_code:content(Node)) > 1 andalso lists:member(NodeType, OpOrMatch)) orelse
        lists:member(NodeType, ExtraOpsTypes).

match_operators() ->
    [match, maybe_match].

no_space({_Config, Target, _RuleConfig} = RuleCfg) ->
    Rules = option(rules, RuleCfg, no_space),
    Root = root(RuleCfg),
    Tokens = ktn_code:attr(tokens, Root),
    TextNodes = lists:filter(fun is_text_node/1, Tokens),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src),
    AllSpaceUntilText =
        [
            {Text,
                re_compile(
                    "^[ ]+" ++
                        re:replace(
                            Text,
                            "(\\.|\\[|\\]|\\^|\\$|\\+|\\*|\\?|\\{|\\}|\\(|\\)|\\||\\\\)",
                            "\\\\\\1",
                            [{return, list}, global]
                        ),
                    [unicode]
                )}
         || {left, Text} <- Rules
        ],
    FlatMap =
        fun(Rule) ->
            check_spaces(Lines, TextNodes, Rule, Encoding, {should_not_have, AllSpaceUntilText})
        end,
    lists:flatmap(FlatMap, Rules).

is_text_node(Node) ->
    ktn_code:attr(text, Node) =/= "".

nesting_level(RuleCfg) ->
    Level = option(level, RuleCfg, nesting_level),

    Root = root(RuleCfg),

    elvis_utils:check_nodes(Root, fun check_nesting_level/2, [Level]).

god_modules(RuleCfg) ->
    Limit = option(limit, RuleCfg, god_modules),

    Root = root(RuleCfg),

    Exports = elvis_code:find_by_types([export], Root),
    Exported = lists:flatmap(fun(Node) -> ktn_code:attr(value, Node) end, Exports),
    case length(Exported) of
        Count when Count > Limit ->
            [
                elvis_result:new_item(
                    "This module's function count is higher than the configured limit",
                    #{limit => Limit}
                )
            ];
        _ ->
            []
    end.

no_if_expression(RuleCfg) ->
    IfExprNodes = elvis_code:find(#{
        of_types => ['if'],
        inside => root(RuleCfg)
    }),

    [
        elvis_result:new_item(
            "an unexpected 'if' expression was found",
            #{node => IfExprNode}
        )
     || IfExprNode <- IfExprNodes
    ].

invalid_dynamic_call(RuleCfg) ->
    Root = root(RuleCfg),

    case elvis_code:find_by_types([callback], Root) of
        [] ->
            check_invalid_dynamic_calls(Root);
        _Callbacks ->
            []
    end.

used_ignored_variable(RuleCfg) ->
    IgnoredVarZippers = elvis_code:find(
        #{
            of_types => [var],
            inside => root(RuleCfg),
            filtered_by => fun is_ignored_var/1,
            filtered_from => zipper
        }
    ),

    [
        elvis_result:new_item(
            "an unexpected use of an ignored variable was found",
            #{zipper => IgnoredVarZipper}
        )
     || IgnoredVarZipper <- IgnoredVarZippers
    ].

no_behavior_info(RuleCfg) ->
    Root = root(RuleCfg),
    Children = ktn_code:content(Root),

    FilterFun =
        fun(Node) ->
            case ktn_code:type(Node) of
                function ->
                    Name = ktn_code:attr(name, Node),
                    lists:member(Name, [behavior_info, behaviour_info]);
                _ ->
                    false
            end
        end,

    BehaviorInfos = lists:filter(FilterFun, Children),
    ResultFun = fun(Node) ->
        elvis_result:new_item(
            "an avoidable 'behavio[u]r_info/1' declaration was found; prefer '-callback' "
            "attributes",
            #{node => Node}
        )
    end,
    lists:map(ResultFun, BehaviorInfos).

module_naming_convention({_Config, Target, _RuleConfig} = RuleCfg) ->
    Regex = option(regex, RuleCfg, module_naming_convention),
    IgnoreModules = option(ignore, RuleCfg, module_naming_convention),

    Root = root(RuleCfg),
    ModuleName =
        case elvis_code:find_by_types([module], Root) of
            [Module] ->
                ktn_code:attr(value, Module);
            _ ->
                elvis_file:module(Target)
        end,

    case lists:member(ModuleName, IgnoreModules) of
        false ->
            ModuleNameStr = atom_to_list(ModuleName),
            case re:run(ModuleNameStr, Regex) of
                nomatch ->
                    [
                        elvis_result:new_item(
                            "The name of this module is not acceptable by regular expression '~p'",
                            [Regex]
                        )
                    ];
                {match, _} ->
                    ForbiddenRegex = option(forbidden_regex, RuleCfg, module_naming_convention),
                    case ForbiddenRegex of
                        undefined ->
                            [];
                        ForbiddenRegex ->
                            is_forbidden_module_name(
                                ModuleNameStr,
                                ForbiddenRegex
                            )
                    end
            end;
        true ->
            []
    end.

is_forbidden_module_name(Target, Regex) ->
    case re:run(Target, Regex, [unicode]) of
        {match, _} ->
            [
                elvis_result:new_item(
                    "The name of this module name is forbidden by regular expression '~p'",
                    [Regex]
                )
            ];
        nomatch ->
            []
    end.

state_record_and_type(RuleCfg) ->
    Root = root(RuleCfg),
    case is_otp_behaviour(Root) of
        true ->
            case {has_state_record(Root), has_state_type(Root)} of
                {true, true} ->
                    [];
                {false, _} ->
                    [
                        elvis_result:new_item(
                            "This module implements an OTP behavior but is missing a '#state{}' "
                            "record"
                        )
                    ];
                {true, false} ->
                    [
                        elvis_result:new_item(
                            "This module implements an OTP behavior and has a '#state{}' record "
                            "but is missing a 'state()' type"
                        )
                    ]
            end;
        false ->
            []
    end.

no_spec_with_records(RuleCfg) ->
    Root = root(RuleCfg),
    SpecNodes = elvis_code:find(fun spec_includes_record/1, Root),
    ResultFun = fun(Node) ->
        elvis_result:new_item(
            "an unexpected record was found in a spec; prefer creating a type for it and "
            "using that",
            #{node => Node}
        )
    end,
    lists:map(ResultFun, SpecNodes).

dont_repeat_yourself(RuleCfg) ->
    MinComplexity = option(min_complexity, RuleCfg, dont_repeat_yourself),

    Root = root(RuleCfg),

    Nodes = find_repeated_nodes(Root, MinComplexity),

    LocationCat =
        fun
            ({Line, Col}, "") ->
                io_lib:format("(~p, ~p)", [Line, Col]);
            ({Line, Col}, Str) ->
                io_lib:format("~s, (~p, ~p)", [Str, Line, Col])
        end,
    ResultFun =
        fun(Locations) ->
            LocationsStr = lists:foldl(LocationCat, "", Locations),
            elvis_result:new_item(
                "The code in the following (<line>, <column>) locations has the same structure: ~p",
                [LocationsStr]
            )
        end,

    lists:map(ResultFun, Nodes).

max_module_length({_Config, Target, _RuleConfig} = RuleCfg) ->
    MaxLength = option(max_length, RuleCfg, max_module_length),
    CountComments = option(count_comments, RuleCfg, max_module_length),
    CountWhitespace = option(count_whitespace, RuleCfg, max_module_length),
    CountDocs = option(count_docs, RuleCfg, max_module_length),

    {Src0, _} = elvis_file:src(Target),

    DocParts = doc_bin_parts(Src0),
    Docs = iolist_to_binary(bin_parts_to_iolist(Src0, DocParts)),
    SrcParts = ignore_bin_parts(Src0, DocParts),
    Src = iolist_to_binary(bin_parts_to_iolist(Src0, SrcParts)),

    FilterFun =
        fun(Line) ->
            (CountComments orelse not line_is_comment(Line)) andalso
                (CountWhitespace orelse not line_is_whitespace(Line))
        end,
    Lines =
        case elvis_utils:split_all_lines(Src, [trim]) of
            Ls when CountComments, CountWhitespace ->
                Ls;
            Ls ->
                lists:filter(FilterFun, Ls)
        end,

    DocLines =
        case CountDocs of
            true ->
                elvis_utils:split_all_lines(Docs, [trim]);
            false ->
                []
        end,

    case length(Lines) + length(DocLines) of
        L when L > MaxLength ->
            [
                elvis_result:new_item(
                    "This module's lines-of-code count is higher than the configured limit",
                    #{limit => MaxLength}
                )
            ];
        _ ->
            []
    end.

max_anonymous_function_arity(RuleCfg) ->
    MaxArity = option(max_arity, RuleCfg, max_anonymous_function_arity),
    Root = root(RuleCfg),
    IsFun =
        fun(Node) ->
            %% Not having clauses means it's something like fun mod:f/10 and we don't want
            %% this rule to raise warnings for those. max_function_arity should take care of them.
            ktn_code:type(Node) =:= 'fun' andalso [] =/= elvis_code:find_by_types([clause], Node)
        end,
    Funs = elvis_code:find(IsFun, Root),
    lists:filtermap(
        fun(Fun) ->
            [FirstClause | _] = elvis_code:find_by_types([clause], Fun),
            case length(ktn_code:node_attr(pattern, FirstClause)) of
                Arity when Arity =< MaxArity ->
                    false;
                _Arity ->
                    {true,
                        elvis_result:new_item(
                            "the arity of the anonymous function is higher than the configured "
                            "limit",
                            #{node => Fun, limit => MaxArity}
                        )}
            end
        end,
        Funs
    ).

max_function_arity(RuleCfg) ->
    ExportedMaxArity = option(max_arity, RuleCfg, max_function_arity),
    NonExportedMaxArity =
        specific_or_default(
            option(non_exported_max_arity, RuleCfg, max_function_arity),
            ExportedMaxArity
        ),
    Root = root(RuleCfg),
    Functions = elvis_code:find_by_types([function], Root),
    lists:filtermap(
        fun(#{attrs := #{arity := Arity, name := Name}} = Function) ->
            Exports = elvis_code:find_by_types([export], Root),
            ExportedFunctions = lists:flatmap(fun(Node) -> ktn_code:attr(value, Node) end, Exports),
            IsExported =
                lists:member({Name, Arity}, ExportedFunctions),
            MaxArity =
                case IsExported of
                    true ->
                        ExportedMaxArity;
                    false ->
                        NonExportedMaxArity
                end,
            case ktn_code:attr(arity, Function) of
                Arity when Arity =< MaxArity ->
                    false;
                Arity ->
                    Name = ktn_code:attr(name, Function),
                    {true,
                        elvis_result:new_item(
                            "the arity of function '~p/~p' is higher than the configured limit",
                            [Name, Arity],
                            #{node => Function, limit => MaxArity}
                        )}
            end
        end,
        Functions
    ).

max_function_clause_length({_Config, Target, _RuleConfig} = RuleCfg) ->
    MaxLength = option(max_length, RuleCfg, max_function_length),
    CountComments = option(count_comments, RuleCfg, max_function_length),
    CountWhitespace = option(count_whitespace, RuleCfg, max_function_length),

    Root = root(RuleCfg),
    {Src, _} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    Functions0 = elvis_code:find_by_types([function], Root),

    % clause
    FilterClause =
        fun(Line) ->
            (CountComments orelse not line_is_comment(Line)) andalso
                (CountWhitespace orelse not line_is_whitespace(Line))
        end,

    PairClause =
        fun(ClauseNode, {Result, PrevAccNum}) ->
            {Min, Max} = node_line_limits(ClauseNode),
            FunLines = lists:sublist(Lines, Min, Max - Min + 1),
            FilteredLines = lists:filter(FilterClause, FunLines),
            L = length(FilteredLines),
            AccNum = PrevAccNum + 1,
            ClauseNumber = parse_clause_num(AccNum),
            {[{Min, ClauseNumber, L} | Result], AccNum}
        end,

    % fun
    PairFun =
        fun(FunctionNode) ->
            Name = ktn_code:attr(name, FunctionNode),
            Arity = ktn_code:attr(arity, FunctionNode),

            Clauses = elvis_code:find_by_types([clause], FunctionNode),

            {ClauseLenInfos, _} = lists:foldl(PairClause, {[], 0}, Clauses),

            [
                {Name, Arity, Min, StringClauseNumber, L}
             || {Min, StringClauseNumber, L} <- ClauseLenInfos
            ]
        end,

    ClauseLenInfos =
        lists:reverse(
            lists:append(
                lists:map(PairFun, Functions0)
            )
        ),

    MaxLengthPred = fun({_, _, _, _, L}) -> L > MaxLength end,
    ClauseLenMaxPairs = lists:filter(MaxLengthPred, ClauseLenInfos),

    ResultFun =
        fun({Name, Arity, StartPos, ClauseNumber, L}) ->
            elvis_result:new_item(
                "the code for the ~p clause of function '~p/~p' has ~p lines, which is higher than "
                "the configured limit",
                [ClauseNumber, Name, Arity, L],
                #{line => StartPos, limit => MaxLength}
            )
        end,
    lists:map(ResultFun, ClauseLenMaxPairs).

parse_clause_num(Num) when Num rem 100 >= 11, Num rem 100 =< 13 ->
    integer_to_list(Num) ++ "th";
parse_clause_num(Num) when Num rem 10 =:= 1 ->
    integer_to_list(Num) ++ "st";
parse_clause_num(Num) when Num rem 10 =:= 2 ->
    integer_to_list(Num) ++ "nd";
parse_clause_num(Num) when Num rem 10 =:= 3 ->
    integer_to_list(Num) ++ "rd";
parse_clause_num(Num) ->
    integer_to_list(Num) ++ "th".

max_function_length({_Config, Target, _RuleConfig} = RuleCfg) ->
    MaxLength = option(max_length, RuleCfg, max_function_length),
    CountComments = option(count_comments, RuleCfg, max_function_length),
    CountWhitespace = option(count_whitespace, RuleCfg, max_function_length),

    Root = root(RuleCfg),
    {Src, _} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    Functions0 = elvis_code:find_by_types([function], Root),
    FilterFun =
        fun(Line) ->
            (CountComments orelse not line_is_comment(Line)) andalso
                (CountWhitespace orelse not line_is_whitespace(Line))
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
            elvis_result:new_item(
                "the code for function '~p/~p' has ~p lines, which is higher than the configured "
                "limit",
                [Name, Arity, L],
                #{line => StartPos, limit => MaxLength}
            )
        end,
    lists:map(ResultFun, FunLenMaxPairs).

no_call(RuleCfg) ->
    DefaultFns = option(no_call_functions, RuleCfg, no_call),
    Msg = "an unexpected call to '~p:~p/~p' was found (check no_call list)",
    no_call_common(RuleCfg, DefaultFns, Msg).

no_debug_call(RuleCfg) ->
    DefaultFns = option(debug_functions, RuleCfg, no_debug_call),
    Msg = "an unexpected debug call to '~p:~p/~p' was found",
    no_call_common(RuleCfg, DefaultFns, Msg).

no_common_caveats_call(RuleCfg) ->
    DefaultFns = option(caveat_functions, RuleCfg, no_common_caveats_call),
    Msg = "the call to '~p:~p/~p' might have performance drawbacks or implicit behavior",
    no_call_common(RuleCfg, DefaultFns, Msg).

-spec node_line_limits(ktn_code:tree_node()) -> {Min :: integer(), Max :: integer()}.
node_line_limits(FunctionNode) ->
    Zipper = elvis_code:code_zipper(FunctionNode),
    % The first number in `lineNums' list is the location of the first
    % line of the function. That's why we use it for the `Min' value.
    LineNums = zipper:map(fun line/1, Zipper),
    % Last function's line
    Max = lists:max(LineNums),
    % If you use `lists:min/1' here, you will get weird results when using
    % macros because most of the time macros are defined at the beginning of
    % the module, but your function's first line could be in the middle or
    % even at the end of the module.

    % Min = first function's line
    [Min | _] = LineNums,
    {Min, Max}.

no_nested_try_catch(RuleCfg) ->
    Root = root(RuleCfg),
    ResultFun = fun(Node) ->
        elvis_result:new_item(
            "an unexpected nested 'try...catch' expression was found",
            #{node => Node}
        )
    end,
    TryExprs = elvis_code:find_by_types(['try'], Root),
    lists:flatmap(fun(TryExp) -> check_nested_try_catchs(ResultFun, TryExp) end, TryExprs).

no_successive_maps(RuleCfg) ->
    Root = root(RuleCfg),
    ResultFun = fun(Node) ->
        elvis_result:new_item(
            "an unexpected map update after map construction/update was found",
            #{node => Node}
        )
    end,
    MapExprs = elvis_code:find_by_types([map], Root, undefined, #{traverse => all}),
    lists:flatmap(fun(MapExp) -> check_successive_maps(ResultFun, MapExp) end, MapExprs).

atom_naming_convention(RuleCfg) ->
    Root = root(RuleCfg),
    Regex = option(regex, RuleCfg, atom_naming_convention),
    ForbiddenRegex = option(forbidden_regex, RuleCfg, atom_naming_convention),
    RegexEnclosed =
        specific_or_default(
            option(enclosed_atoms, RuleCfg, atom_naming_convention),
            Regex
        ),
    ForbiddenEnclosedRegex =
        specific_or_default(
            option(forbidden_enclosed_regex, RuleCfg, atom_naming_convention),
            ForbiddenRegex
        ),
    IsAtomNode = fun(Node) ->
        ktn_code:type(zipper:node(Node)) =:= atom andalso not check_parent_remote(Node)
    end,
    AtomZippers = elvis_code:find(IsAtomNode, Root, #{traverse => all, filtered_from => zipper}),
    check_atom_names(
        Regex,
        ForbiddenRegex,
        RegexEnclosed,
        ForbiddenEnclosedRegex,
        AtomZippers,
        []
    ).

no_init_lists(RuleCfg) ->
    Root = root(RuleCfg),

    ListInitClauses =
        case is_relevant_behaviour(Root, RuleCfg) of
            true ->
                IsInit1Function =
                    fun(Node) ->
                        ktn_code:type(Node) =:= function andalso
                            ktn_code:attr(name, Node) =:= init andalso
                            ktn_code:attr(arity, Node) =:= 1
                    end,

                case elvis_code:find(IsInit1Function, Root) of
                    [] ->
                        [];
                    [Init1Fun] ->
                        Content = ktn_code:content(Init1Fun),
                        ListAttrClauses =
                            lists:filtermap(fun filter_list_clause/1, Content),
                        case length(ListAttrClauses) =:= length(Content) of
                            true ->
                                ListAttrClauses;
                            false ->
                                []
                        end
                end;
            false ->
                []
        end,

    ResultFun =
        fun(Node) ->
            elvis_result:new_item(
                "an avoidable list was found as argumeent to 'init' callback; prefer tuples, maps "
                "or records",
                #{node => Node}
            )
        end,

    lists:map(ResultFun, ListInitClauses).

is_relevant_behaviour(Root, RuleCfg) ->
    ConfigBehaviors = option(behaviours, RuleCfg, no_init_lists),
    Behaviours = elvis_code:find_by_types([behaviour, behavior], Root),
    lists:any(
        fun(BehaviourNode) ->
            lists:member(
                ktn_code:attr(value, BehaviourNode), ConfigBehaviors
            )
        end,
        Behaviours
    ).

filter_list_clause(Clause) ->
    [Attribute] = ktn_code:node_attr(pattern, Clause),
    case is_list_node(Attribute) of
        true ->
            {true, Clause};
        false ->
            false
    end.

is_list_node(#{type := cons}) ->
    true;
is_list_node(#{type := nil}) ->
    true;
is_list_node(#{type := match, content := Content}) ->
    lists:any(fun(Elem) -> is_list_node(Elem) end, Content);
is_list_node(_) ->
    false.

ms_transform_included(RuleCfg) ->
    Root = root(RuleCfg),

    Functions = get_fun_2_ms_calls(Root),

    IsIncluded = Functions =/= [] andalso has_include_ms_transform(Root),

    case IsIncluded of
        true ->
            [];
        false ->
            ResultFun =
                fun(Function) ->
                    elvis_result:new_item(
                        "'ets:fun2ms/1' is used but the module is missing "
                        "'-include_lib(\"stdlib/include/ms_transform.hrl\").'",
                        #{node => Function}
                    )
                end,
            lists:map(ResultFun, Functions)
    end.

-spec get_fun_2_ms_calls(ktn_code:tree_node()) -> [term()].
get_fun_2_ms_calls(Root) ->
    IsFun2MsFunctionCall =
        fun(Node) -> is_call(Node) andalso is_ets_fun2ms(Node) end,

    elvis_code:find(IsFun2MsFunctionCall, Root).

-spec is_ets_fun2ms(ktn_code:tree_node()) -> boolean().
is_ets_fun2ms(Node) ->
    Fun = ktn_code:node_attr(function, Node),
    Fun2 = ktn_code:node_attr(function, Fun),
    Module = ktn_code:node_attr(module, Fun),

    ktn_code:attr(value, Module) =:= ets andalso ktn_code:attr(value, Fun2) =:= fun2ms.

no_boolean_in_comparison(RuleCfg) ->
    Root = root(RuleCfg),

    IsBoolean =
        fun(Node) ->
            is_boolean(ktn_code:attr(value, Node))
        end,

    IsComparisonWithBoolean =
        fun(Node) ->
            Content = ktn_code:content(Node),
            ktn_code:type(Node) =:= op andalso
                lists:member(ktn_code:attr(operation, Node), ['==', '=:=', '/=', '=/=']) andalso
                lists:any(IsBoolean, Content)
        end,
    ComparisonsWithBoolean =
        lists:uniq(
            elvis_code:find(IsComparisonWithBoolean, Root, #{traverse => all})
        ),

    ResultFun =
        fun(Node) ->
            elvis_result:new_item(
                "an avoidable comparison to boolean was found",
                #{node => Node}
            )
        end,

    lists:map(ResultFun, ComparisonsWithBoolean).

no_receive_without_timeout(RuleCfg) ->
    Root = root(RuleCfg),

    Receives = elvis_code:find_by_types(['receive'], Root),

    ReceivesWithoutTimeout = lists:filter(fun is_receive_without_timeout/1, Receives),

    ResultFun =
        fun(Node) ->
            elvis_result:new_item(
                "a 'receive' expression was found without an 'after' clause; "
                "prefer to include 'after' in 'receive' expressions",
                #{node => Node}
            )
        end,

    lists:map(ResultFun, ReceivesWithoutTimeout).

is_receive_without_timeout(Receive) ->
    [] == elvis_code:find_by_types([receive_after], Receive).

no_operation_on_same_value(RuleCfg) ->
    Root = root(RuleCfg),
    InterestingOps = option(operations, RuleCfg, no_operation_on_same_value),

    IsInterestingOp =
        fun(Node) ->
            ktn_code:type(Node) =:= op andalso
                lists:member(ktn_code:attr(operation, Node), InterestingOps)
        end,

    OpNodes =
        lists:uniq(
            elvis_code:find(IsInterestingOp, Root, #{traverse => all})
        ),

    BadOpNodes = lists:filter(fun same_value_on_both_sides/1, OpNodes),

    ResultFun =
        fun(Node) ->
            elvis_result:new_item(
                "redundant operation '~p' has the same value on both sides",
                [ktn_code:attr(operation, Node)],
                #{node => Node}
            )
        end,

    lists:map(ResultFun, BadOpNodes).

same_value_on_both_sides(Node) ->
    case ktn_code:content(Node) of
        [Left, Right] ->
            same_except_location_attr(Left, Right);
        _ ->
            false
    end.

same_except_location_attr([], []) ->
    true;
same_except_location_attr([LeftNode | LeftNodes], [RightNode | RigthNodes]) ->
    same_except_location_attr(LeftNode, RightNode) andalso
        same_except_location_attr(LeftNodes, RigthNodes);
same_except_location_attr(LeftNode, RightNode) ->
    %% If we're evaluating a function, then even if we evaluate the same function on both sides,
    %% the results may be different.
    not is_call(LeftNode) andalso
        not is_call(RightNode) andalso
        ktn_code:type(LeftNode) =:= ktn_code:type(RightNode) andalso
        maps:remove(location, maps:get(attrs, LeftNode)) =:=
            maps:remove(location, maps:get(attrs, RightNode)) andalso
        same_node_attrs_except_location(LeftNode, RightNode) andalso
        same_except_location_attr(ktn_code:content(LeftNode), ktn_code:content(RightNode)).

same_node_attrs_except_location(#{node_attrs := LeftAttrs}, #{node_attrs := RightAttrs}) ->
    maps:keys(LeftAttrs) =:= maps:keys(RightAttrs) andalso
        lists:all(
            fun(AttrKey) ->
                same_except_location_attr(
                    maps:get(AttrKey, LeftAttrs), maps:get(AttrKey, RightAttrs)
                )
            end,
            maps:keys(LeftAttrs)
        );
same_node_attrs_except_location(LeftNode, RightNode) ->
    not maps:is_key(node_attrs, LeftNode) andalso not maps:is_key(node_attrs, RightNode).

-spec has_include_ms_transform(ktn_code:tree_node()) -> boolean().
has_include_ms_transform(Root) ->
    Fun = fun(Node) ->
        ktn_code:type(Node) =:= include_lib andalso
            ktn_code:attr(value, Node) =:= "stdlib/include/ms_transform.hrl"
    end,

    elvis_code:find(Fun, Root) =/= [].

no_throw(RuleCfg) ->
    Zipper =
        fun(Node) ->
            lists:any(fun(T) -> is_call(Node, T) end, [{throw, 1}, {erlang, throw, 1}])
        end,
    Root = root(RuleCfg),
    ThrowNodes = elvis_code:find(Zipper, Root),
    lists:map(
        fun(ThrowNode) ->
            elvis_result:new_item(
                "an avoidable call to 'throw/1' was found; prefer 'exit/1' or 'error/1'",
                #{node => ThrowNode}
            )
        end,
        ThrowNodes
    ).

no_dollar_space(RuleCfg) ->
    IsDollarSpace =
        fun(Node) -> ktn_code:type(Node) =:= char andalso ktn_code:attr(text, Node) =:= "$ " end,
    Root = root(RuleCfg),
    DollarSpaceNodes = elvis_code:find(IsDollarSpace, Root, #{traverse => all}),
    lists:map(
        fun(ThrowNode) ->
            elvis_result:new_item(
                "unexpected character '$ ' was found; prefer $\\s",
                #{node => ThrowNode}
            )
        end,
        DollarSpaceNodes
    ).

no_author(RuleCfg) ->
    Root = root(RuleCfg),
    Nodes = elvis_code:find_by_types([author], Root),
    lists:map(
        fun(Node) ->
            elvis_result:new_item(
                "avoidable attribute '-author' was found",
                #{node => Node}
            )
        end,
        Nodes
    ).

no_import(RuleCfg) ->
    Root = root(RuleCfg),
    Nodes = elvis_code:find_by_types([import], Root),
    lists:map(
        fun(Node) ->
            elvis_result:new_item(
                "unexpected attribute '-import' was found",
                #{node => Node}
            )
        end,
        Nodes
    ).

no_catch_expressions(RuleCfg) ->
    Root = root(RuleCfg),
    CatchNodes = elvis_code:find_by_types(['catch'], Root),
    lists:map(
        fun(CatchNode) ->
            elvis_result:new_item(
                "an unexpected 'catch' expression was found; prefer a 'try' expression",
                #{node => CatchNode}
            )
        end,
        CatchNodes
    ).

no_single_clause_case(RuleCfg) ->
    Root = root(RuleCfg),
    IsSingleClauseCaseExpression = fun(Node) ->
        ktn_code:type(Node) =:= 'case' andalso length(case_clauses_in(Node)) =:= 1
    end,
    CaseNodes = elvis_code:find(IsSingleClauseCaseExpression, Root),
    lists:map(
        fun(CaseNode) ->
            elvis_result:new_item(
                "an avoidable single-clause 'case' expression was found",
                #{node => CaseNode}
            )
        end,
        CaseNodes
    ).

case_clauses_in(Node) ->
    [
        Clause
     || SubNode <- ktn_code:content(Node),
        ktn_code:type(SubNode) =:= case_clauses,
        Clause <- ktn_code:content(SubNode)
    ].

no_single_match_maybe(RuleCfg) ->
    Root = root(RuleCfg),
    IsSingleMatchMaybeBlock = fun(Node) ->
        ktn_code:type(Node) =:= 'maybe' andalso length(ktn_code:content(Node)) =:= 1
    end,
    CaseNodes = elvis_code:find(IsSingleMatchMaybeBlock, Root),
    lists:map(
        fun(CaseNode) ->
            elvis_result:new_item(
                "an avoidable single-match 'maybe' block was found",
                #{node => CaseNode}
            )
        end,
        CaseNodes
    ).

no_match_in_condition(RuleCfg) ->
    Root = root(RuleCfg),
    CaseNodes = elvis_code:find(fun is_match_in_condition/1, Root),
    lists:map(
        fun(CaseNode) ->
            elvis_result:new_item(
                "an avoidable match condition in a 'case' expression was found; prefer matching "
                "in 'case' clauses",
                #{node => CaseNode}
            )
        end,
        CaseNodes
    ).

is_match_in_condition(Node) ->
    ktn_code:type(Node) =:= case_expr andalso
        %% case_expr followed by a match
        (has_match_child(Node) orelse
            %% or case_expr followed by a block which contains a match in the first layer
            lists:any(
                fun(Node1) ->
                    ktn_code:type(Node1) =:= block andalso has_match_child(Node1)
                end,
                ktn_code:content(Node)
            )).

is_match(Node) ->
    lists:member(ktn_code:type(Node), match_operators()).

has_match_child(Node) ->
    lists:any(fun is_match/1, ktn_code:content(Node)).

numeric_format(RuleCfg) ->
    Root = root(RuleCfg),
    Regex = option(regex, RuleCfg, numeric_format),
    IntRegex = specific_or_default(option(int_regex, RuleCfg, numeric_format), Regex),
    FloatRegex = specific_or_default(option(float_regex, RuleCfg, numeric_format), Regex),
    IntNodes = elvis_code:find_by_types([integer], Root),
    FloatNodes = elvis_code:find_by_types([float], Root),
    check_numeric_format(
        IntRegex,
        IntNodes,
        check_numeric_format(FloatRegex, FloatNodes, [])
    ).

behaviour_spelling(RuleCfg) ->
    Spelling = option(spelling, RuleCfg, behaviour_spelling),
    Root = root(RuleCfg),
    IsWronglySpelledBehaviour =
        fun(Node) ->
            (ktn_code:type(Node) =:= behaviour orelse ktn_code:type(Node) =:= behavior) andalso
                ktn_code:type(Node) =/= Spelling
        end,
    InconsistentBehaviorNodes = elvis_code:find(IsWronglySpelledBehaviour, Root),
    ResultFun =
        fun(Node) ->
            elvis_result:new_item(
                "an unexpected spelling of 'behavio[u]r' was found; prefer ~p",
                [Spelling],
                #{node => Node}
            )
        end,
    lists:map(ResultFun, InconsistentBehaviorNodes).

param_pattern_matching(RuleCfg) ->
    Side = option(side, RuleCfg, param_pattern_matching),
    Root = root(RuleCfg),

    FunctionClausePatterns =
        lists:flatmap(
            fun(Clause) -> ktn_code:node_attr(pattern, zipper:node(Clause)) end,
            elvis_code:find(
                fun is_function_clause/1,
                Root,
                #{filtered_from => zipper, traverse => all}
            )
        ),

    IsMatch = fun(Node) -> ktn_code:type(Node) =:= match end,
    MatchesInFunctionClauses = lists:filter(IsMatch, FunctionClausePatterns),

    lists:filtermap(
        fun(Match) ->
            R =
                case lists:map(fun ktn_code:type/1, ktn_code:content(Match)) of
                    [var, var] ->
                        false;
                    [var, _] when Side =:= right ->
                        [Var0, _] = ktn_code:content(Match),
                        {true, {Side, Var0}};
                    [_, var] when Side =:= left ->
                        [_, Var0] = ktn_code:content(Match),
                        {true, {Side, Var0}};
                    _ ->
                        false
                end,
            case R of
                false ->
                    false;
                {true, {Side, Var}} ->
                    {true,
                        elvis_result:new_item(
                            "variable '~p' is used to match an argument, but placed on "
                            "the wrong side of it; prefer the ~p side",
                            [ktn_code:attr(name, Var), Side],
                            #{node => Match}
                        )}
            end
        end,
        MatchesInFunctionClauses
    ).

is_function_clause(Zipper) ->
    Node = zipper:node(Zipper),
    ktn_code:type(Node) =:= clause andalso is_function_or_fun(zipper:up(Zipper)).

is_function_or_fun(Zipper) ->
    Node = zipper:node(Zipper),
    ktn_code:type(Node) =:= function orelse ktn_code:type(Node) =:= 'fun'.

consistent_generic_type(RuleCfg) ->
    TypePreference = option(preferred_type, RuleCfg, consistent_generic_type),
    Root = root(RuleCfg),
    IsInconsistentGenType = consistent_generic_type_predicate(TypePreference),
    InconsistentTypeNodes = elvis_code:find(IsInconsistentGenType, Root, #{traverse => all}),
    ResultFun = consistent_generic_type_result(TypePreference),
    lists:map(ResultFun, InconsistentTypeNodes).

always_shortcircuit(RuleCfg) ->
    Operators = #{'and' => 'andalso', 'or' => 'orelse'},
    Root = root(RuleCfg),
    IsBadOperator =
        fun(Node) ->
            is_operator_node(Node) andalso
                lists:member(
                    ktn_code:attr(operation, Node), maps:keys(Operators)
                )
        end,
    BadOperators = elvis_code:find(IsBadOperator, Root, #{traverse => all}),
    ResultFun =
        fun(Node) ->
            BadOperator = ktn_code:attr(operation, Node),
            GoodOperator = maps:get(BadOperator, Operators),
            elvis_result:new_item(
                "unexpected non-shortcircuiting operator '~p' was found; prefer ~p",
                [BadOperator, GoodOperator],
                #{node => Node}
            )
        end,
    lists:map(ResultFun, BadOperators).

export_used_types(RuleCfg) ->
    Root = root(RuleCfg),
    case is_otp_behaviour(Root) of
        false ->
            export_used_types_in(Root);
        true ->
            []
    end.

export_used_types_in(TreeRootNode) ->
    FunctionExports = elvis_code:find_by_types([export], TreeRootNode),
    ExportedFunctions = lists:flatmap(fun(Node) -> ktn_code:attr(value, Node) end, FunctionExports),
    SpecNodes = elvis_code:find_by_types([spec], TreeRootNode),
    ExportedSpecs =
        lists:filter(
            fun
                (#{attrs := #{arity := Arity, name := Name}}) ->
                    lists:member({Name, Arity}, ExportedFunctions);
                (_) ->
                    false
            end,
            SpecNodes
        ),
    UsedTypes =
        lists:usort(
            lists:flatmap(
                fun(Spec) ->
                    Types = elvis_code:find_by_types([user_type], Spec, undefined, #{
                        traverse => all
                    }),
                    % yes, on a -type line, the arity is based on `args`, but on
                    % a -spec line, it's based on `content`
                    [
                        {Name, length(Vars)}
                     || #{attrs := #{name := Name}, content := Vars} <- Types
                    ]
                end,
                ExportedSpecs
            )
        ),
    TypeExports = elvis_code:find_by_types([export_type], TreeRootNode),
    ExportedTypes = lists:flatmap(fun(Node) -> ktn_code:attr(value, Node) end, TypeExports),
    UnexportedUsedTypes = lists:subtract(UsedTypes, ExportedTypes),
    Locations = map_type_declarations_to_location(TreeRootNode),

    % Report
    lists:map(
        fun({Name, Arity} = Info) ->
            {Line, Column} = maps:get(Info, Locations, {-1, -1}),
            elvis_result:new_item(
                "type '~p/~p' is used by an exported function; prefer to also export the type",
                [Name, Arity],
                #{line => Line, column => Column}
            )
        end,
        UnexportedUsedTypes
    ).

get_type_of_type(#{
    type := type_attr,
    node_attrs := #{type := #{attrs := #{name := TypeOfType}}}
}) ->
    TypeOfType;
get_type_of_type(_) ->
    undefined.

private_data_types(RuleCfg) ->
    TypesToCheck = option(apply_to, RuleCfg, private_data_types),
    TreeRootNode = root(RuleCfg),
    TypeExports = elvis_code:find_by_types([export_type], TreeRootNode),
    ExportedTypes = lists:flatmap(fun(Node) -> ktn_code:attr(value, Node) end, TypeExports),
    Locations = map_type_declarations_to_location(TreeRootNode),

    PublicDataTypes = public_data_types(TypesToCheck, TreeRootNode, ExportedTypes),

    lists:map(
        fun({Name, Arity} = Info) ->
            {Line, Column} = maps:get(Info, Locations, {-1, -1}),
            elvis_result:new_item(
                "private data type '~p/~p' is exported; prefer not exporting it or making it "
                "opaque",
                [Name, Arity],
                #{line => Line, column => Column}
            )
        end,
        PublicDataTypes
    ).

public_data_types(TypesToCheck, TreeRootNode, ExportedTypes) ->
    Fun = fun(Node) -> lists:member(get_type_of_type(Node), TypesToCheck) end,
    Types =
        [
            name_arity_from_type_line(Node)
         || Node <- elvis_code:find(Fun, TreeRootNode, #{traverse => all})
        ],
    lists:filter(fun({Name, Arity}) -> lists:member({Name, Arity}, ExportedTypes) end, Types).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec name_arity_from_type_line(ktn_code:tree_node()) -> {atom(), integer()}.
name_arity_from_type_line(#{attrs := #{name := Name}, node_attrs := #{args := Args}}) ->
    {Name, length(Args)}.

-spec map_type_declarations_to_location(ktn_code:tree_node()) ->
    #{{atom(), number()} => number()}.
map_type_declarations_to_location(TreeRootNode) ->
    AllTypes = elvis_code:find_by_types([type_attr], TreeRootNode),
    lists:foldl(
        fun
            (
                #{
                    attrs := #{location := Location, name := Name},
                    node_attrs := #{args := Args}
                },
                Acc
            ) ->
                maps:put({Name, length(Args)}, Location, Acc);
            (_, Acc) ->
                Acc
        end,
        #{},
        AllTypes
    ).

specific_or_default(same, Regex) ->
    Regex;
specific_or_default(RegexEnclosed, _Regex) ->
    RegexEnclosed.

check_numeric_format(_Regex, [], Acc) ->
    lists:reverse(Acc);
check_numeric_format(Regex, [NumNode | RemainingNumNodes], AccIn) ->
    AccOut =
        case ktn_code:attr(text, NumNode) of
            undefined ->
                AccIn;
            Number ->
                case re:run(Number, Regex) of
                    nomatch ->
                        [
                            elvis_result:new_item(
                                "the format of number '~p' is not acceptable by regular expression "
                                "'~p'",
                                [Number, Regex],
                                #{node => NumNode}
                            )
                            | AccIn
                        ];
                    {match, _} ->
                        AccIn
                end
        end,
    check_numeric_format(Regex, RemainingNumNodes, AccOut).

is_exception_or_non_reversible(error) ->
    true;
is_exception_or_non_reversible(exit) ->
    true;
is_exception_or_non_reversible(throw) ->
    true;
is_exception_or_non_reversible(non_reversible_form) ->
    true;
is_exception_or_non_reversible(_) ->
    false.

check_atom_names(_Regex, _, _RegexEnclosed, _, [] = _AtomZippers, Acc) ->
    Acc;
check_atom_names(
    Regex,
    ForbiddenRegex,
    RegexEnclosed,
    ForbiddenRegexEnclosed,
    [AtomZipper | RemainingAtomZippers],
    AccIn
) ->
    AtomNode = zipper:node(AtomZipper),
    AtomName0 = ktn_code:attr(text, AtomNode),
    ValueAtomName = ktn_code:attr(value, AtomNode),
    {IsEnclosed, AtomName} = string_strip_enclosed(AtomName0),
    IsExceptionClass = is_exception_or_non_reversible(ValueAtomName),
    RE = re_compile_for_atom_type(IsEnclosed, Regex, RegexEnclosed),
    REF = re_compile_for_atom_type(IsEnclosed, ForbiddenRegex, ForbiddenRegexEnclosed),
    AtomNameUnicode = unicode:characters_to_list(AtomName),
    AccOut =
        case re:run(AtomNameUnicode, RE) of
            _ when IsExceptionClass, not IsEnclosed ->
                [];
            nomatch when not IsEnclosed ->
                [
                    elvis_result:new_item(
                        "the name of atom '~p' is not acceptable by regular expression '~p'",
                        [AtomName0, Regex],
                        #{node => AtomNode}
                    )
                ];
            nomatch when IsEnclosed ->
                [
                    elvis_result:new_item(
                        "the name of enclosed atom '~p' is not acceptable by regular expression "
                        "'~p'",
                        [AtomName0, RegexEnclosed],
                        #{node => AtomNode}
                    )
                ];
            {match, _Captured} when REF =:= undefined ->
                [];
            {match, _Captured} when REF =/= undefined ->
                case re:run(AtomNameUnicode, REF) of
                    _ when IsExceptionClass, not IsEnclosed ->
                        [];
                    {match, _} when not IsEnclosed ->
                        [
                            elvis_result:new_item(
                                "the name of atom '~p' is forbidden by regular expression '~p'",
                                [AtomName, ForbiddenRegex],
                                #{node => AtomNode}
                            )
                        ];
                    {match, _} when IsEnclosed ->
                        [
                            elvis_result:new_item(
                                "the name of enclosed atom '~p' is forbidden by regular "
                                "expression '~p'",
                                [AtomName, ForbiddenRegexEnclosed],
                                #{node => AtomNode}
                            )
                        ];
                    nomatch ->
                        []
                end;
            _ ->
                []
        end,
    check_atom_names(
        Regex,
        ForbiddenRegex,
        RegexEnclosed,
        ForbiddenRegexEnclosed,
        RemainingAtomZippers,
        AccOut ++ AccIn
    ).

string_strip_enclosed([$' | Rest]) ->
    [$' | Reversed] = lists:reverse(Rest),
    IsEnclosed = true,
    EnclosedAtomName = lists:reverse(Reversed),
    {IsEnclosed, EnclosedAtomName};
string_strip_enclosed(NonEnclosedAtomName) ->
    IsEnclosed = false,
    {IsEnclosed, NonEnclosedAtomName}.

re_compile_for_atom_type(false = _IsEnclosed, undefined = _Regex, _RegexEnclosed) ->
    undefined;
re_compile_for_atom_type(true = _IsEnclosed, _Regex, undefined = _RegexEnclosed) ->
    undefined;
re_compile_for_atom_type(false = _IsEnclosed, Regex, _RegexEnclosed) ->
    re_compile(Regex, [unicode]);
re_compile_for_atom_type(true = _IsEnclosed, _Regex, RegexEnclosed) ->
    re_compile(RegexEnclosed, [unicode]).

%% Variables name
check_variables_name(_Regex, _, []) ->
    [];
check_variables_name(Regex, ForbiddenRegex, [VariableZipper | RemainingVars]) ->
    Variable = zipper:node(VariableZipper),
    VariableNameStr = atom_to_list(ktn_code:attr(name, Variable)),
    case re:run(VariableNameStr, Regex) of
        nomatch when VariableNameStr =:= "_" ->
            check_variables_name(Regex, ForbiddenRegex, RemainingVars);
        nomatch ->
            [
                elvis_result:new_item(
                    "the name of variable '~p' is not acceptable by regular expression '~p'",
                    [VariableNameStr, Regex],
                    #{node => Variable}
                )
                | check_variables_name(Regex, ForbiddenRegex, RemainingVars)
            ];
        {match, _} ->
            case ForbiddenRegex of
                undefined ->
                    check_variables_name(Regex, ForbiddenRegex, RemainingVars);
                ForbiddenRegex ->
                    case re:run(VariableNameStr, ForbiddenRegex, [unicode]) of
                        {match, _} ->
                            [
                                elvis_result:new_item(
                                    "the name of variable '~p' is forbidden by regular "
                                    "expression '~p'",
                                    [VariableNameStr, Regex],
                                    #{node => Variable}
                                )
                                | check_variables_name(Regex, ForbiddenRegex, RemainingVars)
                            ];
                        nomatch ->
                            check_variables_name(Regex, ForbiddenRegex, RemainingVars)
                    end
            end
    end.

%%% Rule checking

%% Line Length

-spec line_is_comment(binary()) -> boolean().
line_is_comment(Line) ->
    case re:run(Line, "^[ \t]*%") of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

-spec line_is_whitespace(binary()) -> boolean().
line_is_whitespace(Line) ->
    case re:run(Line, "^[ \t]*$") of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

%% Macro Names

macro_name_from(MacroNode, original) ->
    MacroNodeValue = ktn_code:attr(value, MacroNode),
    MacroAsAtom = macro_as_atom(false, [call, var, atom], MacroNodeValue),
    atom_to_list(MacroAsAtom);
macro_name_from(MacroNode, stripped) ->
    MacroNameOriginal = macro_name_from(MacroNode, original),
    unicode:characters_to_list(string:strip(MacroNameOriginal, both, $')).

macro_as_atom({var, _Text, MacroAsAtom}, _Types, _MacroNodeValue) ->
    MacroAsAtom;
macro_as_atom({atom, _Text, MacroAsAtom}, _Types, _MacroNodeValue) ->
    MacroAsAtom;
macro_as_atom(
    {call, _CallText, {Type, _AtomText, MacroAsAtom}, _VarArg},
    _Types,
    _MacroNodeValue
) when
    Type =:= var; Type =:= atom
->
    MacroAsAtom;
macro_as_atom(false, [Type | OtherTypes], MacroNodeValue) ->
    macro_as_atom(lists:keyfind(Type, _N = 1, MacroNodeValue), OtherTypes, MacroNodeValue).

%% Operator (and Text) Spaces
-spec check_spaces(
    Lines :: [binary()],
    Nodes :: [ktn_code:tree_node()],
    Rule :: {right | left, string()},
    Encoding :: latin1 | utf8,
    How :: {should_have, []} | {should_not_have, [{string(), {ok, _}}]}
) ->
    [elvis_result:item()].
% _ is re:mp()

check_spaces(Lines, UnfilteredNodes, {Position, Text}, Encoding, {How0, _} = How) ->
    FilterFun = fun(Node) ->
        ktn_code:attr(text, Node) =:= Text orelse
            (ktn_code:type(Node) =:= dot andalso Text =:= ".")
    end,
    Nodes = lists:filter(FilterFun, UnfilteredNodes),
    SpaceChar = $\s,
    FlatFun =
        fun(Node) ->
            Location = ktn_code:attr(location, Node),
            case character_at_location(Position, Lines, Text, Location, Encoding, How) of
                Char when Char =:= SpaceChar, How0 =:= should_have ->
                    [];
                Char when Char =/= SpaceChar, How0 =:= should_not_have ->
                    [];
                _ when How0 =:= should_have ->
                    [
                        elvis_result:new_item(
                            "there is a missing space to the ~p of '~p'",
                            [Position, Text],
                            #{node => Node}
                        )
                    ];
                _ when How0 =:= should_not_have ->
                    [
                        elvis_result:new_item(
                            "an unexpected space was found to the ~p of '~p'",
                            [Position, Text],
                            #{node => Node}
                        )
                    ]
            end
        end,
    lists:flatmap(FlatFun, Nodes).

maybe_re_run(_Line, undefined = _Regex) ->
    nomatch;
maybe_re_run(Line, Regex) ->
    re:run(Line, Regex).

-spec character_at_location(
    Position :: atom(),
    Lines :: [binary()],
    Text :: string(),
    Location :: {integer(), integer()},
    Encoding :: latin1 | utf8,
    How :: {should_have, []} | {should_not_have, [{string(), {ok, _}}]}
) ->
    char().
% _ is re:mp()
character_at_location(
    Position,
    Lines,
    Text,
    {LineNo, Col},
    Encoding,
    {How, TextRegexes}
) ->
    Line = lists:nth(LineNo, Lines),
    TextRegex =
        case TextRegexes of
            [] ->
                nomatch;
            _ ->
                maybe_re_run(Line, proplists:get_value(Text, TextRegexes))
        end,
    TextLineStr = unicode:characters_to_list(Line, Encoding),
    ColToCheck =
        case Position of
            left ->
                Col - 1;
            right ->
                Col + length(Text)
        end,
    % If ColToCheck is greater than the length of TextLineStr variable, it
    % means the end of line was reached so return " " (or "") to make the check pass,
    % otherwise return the character at the given column.
    % NOTE: text below only applies when the given Position is equal to `right`,
    %       or Position is equal to `left` and Col is 1.
    SpaceChar = $\s,

    case {ColToCheck, Position, length(TextLineStr)} of
        {0, _, _} when Text =/= ")" ->
            SpaceChar;
        {_, right, LenLine} when How =:= should_have, ColToCheck > LenLine ->
            SpaceChar;
        {_, right, LenLine} when How =:= should_not_have, ColToCheck > LenLine ->
            "";
        _ when How =:= should_have; TextRegex =:= nomatch, ColToCheck > 1 ->
            lists:nth(ColToCheck, TextLineStr);
        _ when
            How =:= should_not_have,
            ColToCheck > 1,
            (Text =:= ":" orelse Text =:= "." orelse Text =:= ";")
        ->
            lists:nth(ColToCheck - 1, TextLineStr);
        _ ->
            ""
    end.

%% Nesting Level
-spec check_nesting_level(ktn_code:tree_node(), [integer()]) -> [elvis_result:item()].
check_nesting_level(ParentNode, [MaxLevel]) ->
    NestedNodes = past_nesting_limit(ParentNode, MaxLevel),
    Fun = fun(Node) ->
        elvis_result:new_item(
            "an expression is nested beyond the configured limit",
            #{node => Node, limit => MaxLevel}
        )
    end,

    lists:map(Fun, NestedNodes).

%% @doc Takes a node and returns all nodes where the nesting limit is exceeded.
-spec past_nesting_limit(ktn_code:tree_node(), integer()) ->
    [{ktn_code:tree_node(), integer()}].
past_nesting_limit(Node, MaxLevel) ->
    ResultNodes = past_nesting_limit(Node, 1, MaxLevel),
    lists:reverse(ResultNodes).

past_nesting_limit(Node, CurrentLevel, MaxLevel) when CurrentLevel > MaxLevel ->
    [Node];
past_nesting_limit(#{content := Content}, CurrentLevel, MaxLevel) ->
    Fun = fun(ChildNode) ->
        Increment = level_increment(ChildNode),
        past_nesting_limit(ChildNode, Increment + CurrentLevel, MaxLevel)
    end,
    lists:flatmap(Fun, Content);
past_nesting_limit(_Node, _CurrentLeve, _MaxLevel) ->
    [].

%% @doc Takes a node and determines its nesting level increment.
level_increment(#{type := 'fun', content := _}) ->
    1;
level_increment(#{type := 'fun'}) ->
    0;
level_increment(#{type := Type}) ->
    IncrementOne = [function, 'case', 'if', try_case, try_catch, named_fun, receive_case],
    case lists:member(Type, IncrementOne) of
        true ->
            1;
        false ->
            0
    end.

%% Invalid Dynamic Calls

-spec check_invalid_dynamic_calls(ktn_code:tree_node()) -> [elvis_result:item()].
check_invalid_dynamic_calls(Root) ->
    InvalidCalls = elvis_code:find(fun is_dynamic_call/1, Root, #{traverse => all}),
    ResultFun = fun(Node) ->
        elvis_result:new_item(
            "an unexpected dynamic function call was found; prefer "
            "making dynamic calls only in modules that define callbacks",
            #{node => Node}
        )
    end,
    lists:map(ResultFun, InvalidCalls).

-spec is_dynamic_call(ktn_code:tree_node()) -> boolean().
is_dynamic_call(Node) ->
    case ktn_code:type(Node) of
        call ->
            FunctionSpec = ktn_code:node_attr(function, Node),
            case ktn_code:type(FunctionSpec) of
                remote ->
                    Module = ktn_code:node_attr(module, FunctionSpec),
                    Function = ktn_code:node_attr(function, FunctionSpec),
                    (ktn_code:type(Module) =/= atom andalso not is_the_module_macro(Module)) orelse
                        ktn_code:type(Function) =/= atom;
                _Other ->
                    false
            end;
        _ ->
            false
    end.

is_the_module_macro(Module) ->
    ktn_code:type(Module) =:= macro andalso ktn_code:attr(name, Module) =:= "MODULE".

%% Plain Variable
-spec is_var(zipper:zipper(_)) -> boolean().
is_var(Zipper) ->
    case
        ktn_code:type(
            zipper:node(Zipper)
        )
    of
        var ->
            PrevLocation =
                case ktn_code:attr(location, zipper:node(Zipper)) of
                    {L, 1} ->
                        {L - 1, 9999};
                    {L, C} ->
                        {L, C - 1}
                end,
            case
                elvis_code:find_token(
                    zipper:root(Zipper), PrevLocation
                )
            of
                not_found ->
                    true;
                {ok, PrevToken} ->
                    ktn_code:type(PrevToken) =/= '?'
            end;
        _NotVar ->
            false
    end.

%% Ignored Variable

-spec is_ignored_var(zipper:zipper(_)) -> boolean().
is_ignored_var(Zipper) ->
    Node = zipper:node(Zipper),
    case ktn_code:type(Node) of
        var ->
            Name = ktn_code:attr(name, Node),
            [FirstChar | _] = atom_to_list(Name),
            (FirstChar =:= $_) andalso (Name =/= '_') andalso
                not check_parent_match_or_macro(Zipper);
        _OtherType ->
            false
    end.

check_parent_match_or_macro(Zipper) ->
    case zipper:up(Zipper) of
        undefined ->
            false;
        ParentZipper ->
            Parent = zipper:node(ParentZipper),
            IsMatchOrMacro = lists:member(ktn_code:type(Parent), [macro | match_operators()]),
            case IsMatchOrMacro of
                true ->
                    zipper:down(ParentZipper) =:= Zipper;
                _ ->
                    check_parent_match_or_macro(ParentZipper)
            end
    end.

check_parent_remote(Zipper) ->
    case zipper:up(Zipper) of
        undefined ->
            false;
        ParentZipper ->
            Parent = zipper:node(ParentZipper),
            ktn_code:type(Parent) =:= remote
    end.

%% State record in OTP module

-spec is_otp_behaviour(ktn_code:tree_node()) -> boolean().
is_otp_behaviour(Root) ->
    OtpSet = sets:from_list([gen_server, gen_event, gen_fsm, gen_statem, supervisor_bridge]),
    case elvis_code:find_by_types([behaviour, behavior], Root) of
        [] ->
            false;
        Behaviors ->
            ValueFun = fun(Node) -> ktn_code:attr(value, Node) end,
            Names = lists:map(ValueFun, Behaviors),
            BehaviorsSet = sets:from_list(Names),
            not sets:is_empty(
                sets:intersection(OtpSet, BehaviorsSet)
            )
    end.

-spec has_state_record(ktn_code:tree_node()) -> boolean().
has_state_record(Root) ->
    IsStateRecord =
        fun(Node) ->
            (ktn_code:type(Node) =:= record_attr) andalso (ktn_code:attr(name, Node) =:= state)
        end,
    elvis_code:find(IsStateRecord, Root) =/= [].

-spec has_state_type(ktn_code:tree_node()) -> boolean().
has_state_type(Root) ->
    IsStateType =
        fun(Node) ->
            case ktn_code:type(Node) of
                type_attr ->
                    ktn_code:attr(name, Node) =:= state;
                opaque ->
                    case ktn_code:attr(value, Node) of
                        {state, _, _} ->
                            true;
                        _ ->
                            false
                    end;
                _ ->
                    false
            end
        end,
    elvis_code:find(IsStateType, Root) =/= [].

%% Spec includes records

-spec spec_includes_record(ktn_code:tree_node()) -> boolean().
spec_includes_record(Node) ->
    IsTypeRecord =
        fun(Child) ->
            (ktn_code:type(Child) =:= type) andalso (ktn_code:attr(name, Child) =:= record)
        end,
    (ktn_code:type(Node) =:= spec) andalso
        (elvis_code:find(IsTypeRecord, Node, #{traverse => all}) =/= []).

%% Don't repeat yourself

-spec find_repeated_nodes(ktn_code:tree_node(), non_neg_integer()) ->
    [ktn_code:tree_node()].
find_repeated_nodes(Root, MinComplexity) ->
    TypeAttrs = #{var => [location, name, text], clause => [location, text]},

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
remove_attrs(
    #{
        attrs := Attrs,
        type := Type,
        node_attrs := NodeAttrs
    } =
        Node,
    TypeAttrs
) ->
    AttrsName = maps:get(Type, TypeAttrs, [location]),
    AttrsNoLoc = maps:without(AttrsName, Attrs),
    NodeAttrsNoLoc =
        [
            {Key, remove_attrs_zipper(elvis_code:code_zipper(Value), TypeAttrs)}
         || {Key, Value} <- maps:to_list(NodeAttrs)
        ],

    Node#{attrs => AttrsNoLoc, node_attrs => maps:from_list(NodeAttrsNoLoc)};
remove_attrs(#{attrs := Attrs, type := Type} = Node, TypeAttrs) ->
    AttrsName = maps:get(Type, TypeAttrs, [location]),
    AttrsNoLoc = maps:without(AttrsName, Attrs),
    Node#{attrs => AttrsNoLoc};
remove_attrs(Node, _TypeAttrs) ->
    Node.

-spec filter_repeated(map()) -> map().
filter_repeated(NodesLocs) ->
    NotRepeated =
        [Node || {Node, LocationSet} <- maps:to_list(NodesLocs), sets:size(LocationSet) =:= 1],

    RepeatedMap = maps:without(NotRepeated, NodesLocs),

    RepeatedNodes = maps:keys(RepeatedMap),
    Nested =
        [
            Node
         || Node <- RepeatedNodes,
            Parent <- RepeatedNodes,
            Node =/= Parent,
            is_children(Parent, Node)
        ],

    maps:without(Nested, RepeatedMap).

is_children(Parent, Node) ->
    Zipper = elvis_code:code_zipper(Parent),
    zipper:filter(fun(Child) -> Child =:= Node end, Zipper) =/= [].

%% No call
no_call_common(RuleCfg, NoCallFuns, Msg) ->
    Root = root(RuleCfg),

    Calls = elvis_code:find_by_types([call], Root),
    check_no_call(Calls, Msg, NoCallFuns).

-spec check_no_call([ktn_code:tree_node()], string(), [
    {module() | '_', atom() | '_', arity() | '_'} | {module() | '_', atom() | '_'}
]) ->
    [elvis_result:item()].
check_no_call(Calls, Msg, NoCallFuns) ->
    BadCalls = [Call || Call <- Calls, is_in_call_list(Call, NoCallFuns)],
    ResultFun =
        fun(Call) ->
            {M, F, A} = call_mfa(Call),
            elvis_result:new_item(
                Msg,
                [M, F, A],
                #{node => Call}
            )
        end,
    lists:map(ResultFun, BadCalls).

is_in_call_list(Call, DisallowedFuns) ->
    MFA = call_mfa(Call),
    MatchFun = fun(Spec) -> fun_spec_match(Spec, MFA) end,
    lists:any(MatchFun, DisallowedFuns).

call_mfa(Call) ->
    FunctionSpec = ktn_code:node_attr(function, Call),
    M0 = ktn_code:attr(value, ktn_code:node_attr(module, FunctionSpec)),
    M =
        case M0 of
            undefined ->
                % this is a bare call, e.g. list_to_atom/1; assume 'erlang'
                erlang;
            _ ->
                M0
        end,
    F0 = ktn_code:attr(value, ktn_code:node_attr(function, FunctionSpec)),
    F =
        case F0 of
            undefined ->
                % this is what we get for local (or erlang:) calls
                ktn_code:attr(value, FunctionSpec);
            _ ->
                F0
        end,
    A = length(ktn_code:content(Call)),
    {M, F, A}.

is_call(Node, {F, A}) ->
    is_call(Node) andalso
        list_to_atom(ktn_code:attr(text, Node)) =:= F andalso
        length(ktn_code:content(Node)) =:= A;
is_call(Node, {M, F, A}) ->
    call_mfa(Node) =:= {M, F, A}.

is_call(Node) ->
    ktn_code:type(Node) =:= call.

fun_spec_match({M, F}, MFA) ->
    fun_spec_match({M, F, '_'}, MFA);
fun_spec_match({M1, F1, A1}, {M2, F2, A2}) ->
    wildcard_match(M1, M2) andalso wildcard_match(F1, F2) andalso wildcard_match(A1, A2).

wildcard_match('_', _) ->
    true;
wildcard_match(X, Y) ->
    X =:= Y.

%% @doc No nested try...catch blocks
check_nested_try_catchs(ResultFun, TryExp) ->
    lists:filtermap(
        fun(Node) ->
            case ktn_code:type(Node) of
                'try' ->
                    {true, ResultFun(Node)};
                _ ->
                    false
            end
        end,
        ktn_code:content(TryExp)
    ).

%% @doc No #{...}#{...}
check_successive_maps(ResultFun, MapExp) ->
    case ktn_code:node_attr(var, MapExp) of
        undefined ->
            [];
        InnerVar ->
            case ktn_code:type(InnerVar) of
                map ->
                    [ResultFun(InnerVar)];
                _ ->
                    []
            end
    end.

%% Consistent Generic Type
consistent_generic_type_predicate(TypePreference) ->
    fun(Node) ->
        NodeName = ktn_code:attr(name, Node),
        (ktn_code:type(Node) =:= type orelse ktn_code:type(Node) =:= callback) andalso
            lists:member(NodeName, [term, any]) andalso
            NodeName =/= TypePreference
    end.

consistent_generic_type_result(TypePreference) ->
    fun(Node) ->
        NodeName = ktn_code:attr(name, Node),
        elvis_result:new_item(
            "unexpected type '~p/0' was found; prefer ~p/0",
            [NodeName, TypePreference],
            #{node => Node}
        )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option(OptionName, RuleCfg, Rule) -> OptionValue when
    OptionName :: atom(),
    RuleCfg :: {Config, Target, RuleConfig},
    Config :: elvis_config:config(),
    Target :: elvis_file:file(),
    RuleConfig :: (Options :: #{atom() => term()}),
    Rule :: atom(),
    OptionValue :: term().
option(OptionName, {_Config, _Target, RuleConfig}, Rule) ->
    maybe_default_option(maps:get(OptionName, RuleConfig, undefined), OptionName, Rule).

-spec maybe_default_option(UserDefinedOptionValue, OptionName, Rule) -> OptionValue when
    UserDefinedOptionValue :: undefined | term(),
    OptionName :: atom(),
    Rule :: atom(),
    OptionValue :: term().
maybe_default_option(undefined = _UserDefinedOptionValue, OptionName, Rule) ->
    maps:get(OptionName, default(Rule));
maybe_default_option(UserDefinedOptionValue, _OptionName, _Rule) ->
    UserDefinedOptionValue.

-spec root({Config, Target, RuleConfig}) -> Res when
    Config :: elvis_config:config(),
    Target :: elvis_file:file(),
    RuleConfig :: (Options :: #{atom() => term()}),
    Res :: ktn_code:tree_node().
root({Config, Target, RuleConfig}) ->
    {Root0, File0} = elvis_file:parse_tree(Config, Target, RuleConfig),
    case maps:get(ruleset, Config, undefined) of
        Ruleset when Ruleset =:= beam_files; Ruleset =:= beam_files_strict ->
            maps:get(abstract_parse_tree, File0);
        _ ->
            Root0
    end.

tokens_as_content(Root) ->
    % Minor trick to have elvis_code assume searches the way it usually does
    #{type => root, content => ktn_code:attr(tokens, Root)}.

-spec doc_bin_parts(Src) -> [Part] when
    Src :: binary(),
    Part :: binary_part().
doc_bin_parts(Src) when is_binary(Src) ->
    RE = "(?ms)^-(?:(moduledoc|doc))\\b\\s*(\"*)?.*?\\2\\.(\\r\\n|\\n)",
    case re:run(Src, RE, [global, {capture, first, index}]) of
        {match, Parts} ->
            [Part || [Part] <- Parts];
        nomatch ->
            []
    end.

-spec ignore_bin_parts(Src, [DocPart]) -> [TextPart] when
    Src :: binary(),
    DocPart :: binary_part(),
    TextPart :: binary_part().
ignore_bin_parts(Src, DocParts) when is_binary(Src), is_list(DocParts) ->
    ignore_bin_parts_1(DocParts, 0, Src).

ignore_bin_parts_1([], Prev, Src) ->
    [{Prev, byte_size(Src) - Prev}];
ignore_bin_parts_1([{Start, Len} | T], Prev, Src) ->
    [{Prev, Start - Prev} | ignore_bin_parts_1(T, Start + Len, Src)].

-spec bin_parts_to_iolist(Src, Parts) -> iolist() when
    Src :: binary(),
    Parts :: [binary_part()].
bin_parts_to_iolist(Src, Parts) when is_binary(Src), is_list(Parts) ->
    [binary_part(Src, Start, Len) || {Start, Len} <- Parts].

line(Node) ->
    {Line, _} = ktn_code:attr(location, Node),
    Line.

re_compile(Regexp, Options) ->
    {ok, MP} = re:compile(Regexp, Options),
    MP.
