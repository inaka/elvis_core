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
    #{regex => "^[A-Z](_?[A-Z0-9]+)*$", forbidden_regex => undefined};
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
    % ) can happen at the start of lines; all others can't
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
default(numeric_format) ->
    #{
        % Not restrictive. Those who want more restrictions can set it like "^[^_]*$"
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
    Regex = option(regex, RuleCfg, ?FUNCTION_NAME),
    ForbiddenRegex = option(forbidden_regex, RuleCfg, ?FUNCTION_NAME),

    FunctionNodes = elvis_code:find(#{
        of_types => [function],
        inside => root(RuleCfg)
    }),

    lists:filtermap(
        fun(FunctionNode) ->
            FunctionName = ktn_code:attr(name, FunctionNode),
            FunctionNameStr = unicode:characters_to_list(atom_to_list(FunctionName)),

            case re_run(FunctionNameStr, Regex) of
                nomatch ->
                    {true,
                        elvis_result:new_item(
                            "the name of function ~p is not acceptable by regular "
                            "expression '~s'",
                            [FunctionNameStr, Regex],
                            #{node => FunctionNode}
                        )};
                {match, _} when ForbiddenRegex =/= undefined ->
                    % We check for forbidden names only after accepted names
                    case re_run(FunctionNameStr, ForbiddenRegex) of
                        {match, _} ->
                            {true,
                                elvis_result:new_item(
                                    "the name of function ~p is forbidden by regular "
                                    "expression '~s'",
                                    [FunctionNameStr, ForbiddenRegex],
                                    #{node => FunctionNode}
                                )};
                        nomatch ->
                            false
                    end;
                _ ->
                    false
            end
        end,
        FunctionNodes
    ).

%% @todo Use maps:groups_from_list/2 when we specify OTP25 as the minimum OTP version
consistent_variable_casing(RuleCfg) ->
    Root = root(RuleCfg),
    {zippers, VarZippers} = elvis_code:find(#{
        of_types => [var],
        inside => Root,
        filtered_by => fun is_var/1,
        filtered_from => zipper,
        traverse => all
    }),
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
                VarZippers
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
                    "variable '~s' (first used in line ~p) is written in "
                    "different ways within the module: ~p",
                    [FirstName, line(FirstVar), OtherNames],
                    #{node => FirstVar}
                )
            ]
    end.

variable_naming_convention(RuleCfg) ->
    Regex = option(regex, RuleCfg, ?FUNCTION_NAME),
    ForbiddenRegex = option(forbidden_regex, RuleCfg, ?FUNCTION_NAME),

    {zippers, VarZippers} = elvis_code:find(#{
        of_types => [var],
        inside => root(RuleCfg),
        filtered_by => fun is_var/1,
        filtered_from => zipper,
        traverse => all
    }),

    lists:filtermap(
        fun(VarZipper) ->
            VarNode = zipper:node(VarZipper),
            VariableNameStr = atom_to_list(ktn_code:attr(name, VarNode)),

            case re_run(VariableNameStr, Regex) of
                nomatch when VariableNameStr =/= "_" ->
                    {true,
                        elvis_result:new_item(
                            "the name of variable ~p is not acceptable by regular "
                            "expression '~s'",
                            [VariableNameStr, Regex],
                            #{node => VarNode}
                        )};
                {match, _} when ForbiddenRegex =/= undefined ->
                    % We check for forbidden names only after accepted names
                    case re_run(VariableNameStr, ForbiddenRegex) of
                        {match, _} ->
                            {true,
                                elvis_result:new_item(
                                    "the name of variable ~p is forbidden by regular "
                                    "expression '~s'",
                                    [VariableNameStr, ForbiddenRegex],
                                    #{node => VarNode}
                                )};
                        nomatch ->
                            false
                    end;
                _ ->
                    false
            end
        end,
        VarZippers
    ).

macro_names(RuleCfg) ->
    Regex = option(regex, RuleCfg, ?FUNCTION_NAME),
    ForbiddenRegex = option(forbidden_regex, RuleCfg, ?FUNCTION_NAME),

    RegexAllow = re_compile(Regex),
    RegexBlock = re_compile(ForbiddenRegex),

    MacroNodes = elvis_code:find(#{
        of_types => [define],
        inside => root(RuleCfg),
        traverse => all
    }),

    lists:filtermap(
        fun(MacroNode) ->
            MacroName = macro_name_from(MacroNode, stripped),

            case re_run(MacroName, RegexAllow) of
                nomatch ->
                    {true,
                        elvis_result:new_item(
                            "the name of macro ~p is not acceptable by "
                            "regular expression '~s'",
                            [macro_name_from(MacroNode, original), Regex],
                            #{node => MacroNode}
                        )};
                {match, _} when RegexBlock =/= undefined ->
                    % We check for forbidden names only after accepted names
                    case re_run(MacroName, RegexBlock) of
                        {match, _} ->
                            {true,
                                elvis_result:new_item(
                                    "the name of macro ~p is forbidden by regular "
                                    "expression '~s'",
                                    [macro_name_from(MacroNode, original), ForbiddenRegex],
                                    #{node => MacroNode}
                                )};
                        nomatch ->
                            false
                    end;
                _ ->
                    false
            end
        end,
        MacroNodes
    ).

no_macros(RuleCfg) ->
    AllowedMacros =
        option(allow, RuleCfg, ?FUNCTION_NAME) ++ eep_predef_macros() ++ logger_macros(),

    MacroNodes = elvis_code:find(#{
        of_types => [macro],
        inside => root(RuleCfg),
        filtered_by =>
            fun(MacroNode) ->
                Macro = list_to_atom(ktn_code:attr(name, MacroNode)),
                not lists:member(Macro, AllowedMacros)
            end
    }),

    [
        elvis_result:new_item(
            "an avoidable macro '~s' was found; prefer no macros",
            [ktn_code:attr(name, MacroNode)],
            #{node => MacroNode}
        )
     || MacroNode <- MacroNodes
    ].

no_types(RuleCfg) ->
    TypeAttrNodes = elvis_code:find(#{
        of_types => [type_attr],
        inside => root(RuleCfg)
    }),

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
    IncludeNodes = elvis_code:find(#{
        of_types => [include, include_lib],
        inside => root(RuleCfg)
    }),

    [
        elvis_result:new_item(
            "unexpected nested '-include[_lib]' attribute ('~s') was found; "
            "avoid including .hrl files in .hrl files",
            [ktn_code:attr(value, IncludeNode)],
            #{node => IncludeNode}
        )
     || IncludeNode <- IncludeNodes
    ].

no_specs(RuleCfg) ->
    SpecNodes = elvis_code:find(#{
        of_types => [spec],
        inside => root(RuleCfg)
    }),

    [
        elvis_result:new_item(
            "an unexpected spec for was found function '~p'; avoid specs in .hrl files",
            [ktn_code:attr(name, SpecNode)],
            #{node => SpecNode}
        )
     || SpecNode <- SpecNodes
    ].

no_block_expressions(RuleCfg) ->
    BlockExprs = elvis_code:find(#{
        of_types => ['begin'],
        inside => tokens_as_content(root(RuleCfg)),
        traverse => all
    }),

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
    TextNodes = elvis_code:find(#{
        of_types => undefined,
        inside => tokens_as_content(root(RuleCfg)),
        filtered_by => fun is_text_node/1
    }),

    {Lines, Encoding} = lines_in(Target),
    generate_space_check_results({Lines, Encoding}, TextNodes, {right, "#"}, {should_not_have, []}).

lines_in(Target) ->
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    {elvis_utils:split_all_lines(Src), Encoding}.

operator_spaces({_Config, Target, _RuleConfig} = RuleCfg) ->
    Rules = option(rules, RuleCfg, ?FUNCTION_NAME),

    Root = root(RuleCfg),

    OpNodes = elvis_code:find(#{
        of_types => undefined,
        inside => Root,
        filtered_by => fun is_operator_node/1
    }),

    PunctuationTokens = elvis_code:find(#{
        of_types => ['=', '&&', ',', ';', dot, '->', ':', '::', '|', '||'],
        inside => tokens_as_content(Root)
    }),

    AllNodes = OpNodes ++ PunctuationTokens,

    {Lines, Encoding} = lines_in(Target),

    lists:flatmap(
        fun(Rule) ->
            generate_space_check_results({Lines, Encoding}, AllNodes, Rule, {should_have, []})
        end,
        Rules
    ).

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
    Rules = option(rules, RuleCfg, ?FUNCTION_NAME),

    TextNodes = elvis_code:find(#{
        of_types => undefined,
        inside => tokens_as_content(root(RuleCfg)),
        filtered_by => fun is_text_node/1
    }),

    AllSpaceUntilText = [
        {Text, re_compile("^[ ]+" ++ escape_regex(Text))}
     || {left, Text} <- Rules
    ],

    {Lines, Encoding} = lines_in(Target),

    lists:flatmap(
        fun(Rule) ->
            generate_space_check_results(
                {Lines, Encoding}, TextNodes, Rule, {should_not_have, AllSpaceUntilText}
            )
        end,
        Rules
    ).

escape_regex(Text) ->
    EscapePattern = "(\\.|\\[|\\]|\\^|\\$|\\+|\\*|\\?|\\{|\\}|\\(|\\)|\\||\\\\)",
    re:replace(Text, EscapePattern, "\\\\\\1", [{return, list}, global]).

is_text_node(Node) ->
    ktn_code:attr(text, Node) =/= "".

nesting_level(RuleCfg) ->
    MaxLevel = option(level, RuleCfg, ?FUNCTION_NAME),

    ParentNodes = elvis_code:find(#{
        of_types => undefined,
        inside => root(RuleCfg)
    }),

    lists:flatmap(
        fun(ParentNode) ->
            NestedNodes = past_nesting_limit(ParentNode, MaxLevel),
            lists:map(
                fun({Node, Level}) ->
                    elvis_result:new_item(
                        "an expression is nested (level = ~p) beyond the configured limit",
                        [Level],
                        #{node => Node, limit => MaxLevel}
                    )
                end,
                NestedNodes
            )
        end,
        ParentNodes
    ).

god_modules(RuleCfg) ->
    Limit = option(limit, RuleCfg, ?FUNCTION_NAME),

    Root = root(RuleCfg),
    ExportedFunctions = exported_functions(Root),

    case length(ExportedFunctions) of
        Count when Count > Limit ->
            [
                elvis_result:new_item(
                    "This module's function count (~p) is higher than the configured limit",
                    [Count],
                    #{limit => Limit}
                )
            ];
        _ ->
            []
    end.

exported_functions(Root) ->
    ExportNodes = elvis_code:find(#{
        of_types => [export],
        inside => Root
    }),
    lists:flatmap(
        fun(Node) ->
            ktn_code:attr(value, Node)
        end,
        ExportNodes
    ).

exported_types(Root) ->
    ExportNodes = elvis_code:find(#{
        of_types => [export_type],
        inside => Root
    }),
    lists:flatmap(
        fun(Node) ->
            ktn_code:attr(value, Node)
        end,
        ExportNodes
    ).

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

    HasCallbacks =
        elvis_code:find(#{
            of_types => [callback],
            inside => Root
        }) =/= [],

    InvalidCallNodes =
        case HasCallbacks of
            true ->
                [];
            false ->
                elvis_code:find(#{
                    of_types => [call],
                    inside => Root,
                    filtered_by => fun is_dynamic_call/1,
                    traverse => all
                })
        end,

    [
        elvis_result:new_item(
            "an unexpected dynamic function call was found; prefer "
            "making dynamic calls only in modules that define callbacks",
            #{node => InvalidCallNode}
        )
     || InvalidCallNode <- InvalidCallNodes
    ].

used_ignored_variable(RuleCfg) ->
    {zippers, IgnoredVarZippers} = elvis_code:find(#{
        of_types => [var],
        inside => root(RuleCfg),
        filtered_by => fun is_ignored_var/1,
        filtered_from => zipper
    }),

    [
        elvis_result:new_item(
            "an unexpected use of an ignored variable was found",
            #{zipper => IgnoredVarZipper}
        )
     || IgnoredVarZipper <- IgnoredVarZippers
    ].

no_behavior_info(RuleCfg) ->
    BehaviourInfoNodes = elvis_code:find(#{
        of_types => [function],
        inside => root(RuleCfg),
        filtered_by =>
            fun(FunctionNode) ->
                FunctionName = ktn_code:attr(name, FunctionNode),
                lists:member(FunctionName, [behavior_info, behaviour_info])
            end
    }),

    [
        elvis_result:new_item(
            "an avoidable 'behavio[u]r_info/1' declaration was found; prefer '-callback' "
            "attributes",
            #{node => BehaviourInfoNode}
        )
     || BehaviourInfoNode <- BehaviourInfoNodes
    ].

module_naming_convention({_Config, Target, _RuleConfig} = RuleCfg) ->
    Regex = option(regex, RuleCfg, ?FUNCTION_NAME),
    ForbiddenRegex = option(forbidden_regex, RuleCfg, ?FUNCTION_NAME),

    RegexAllow = re_compile(Regex),
    RegexBlock = re_compile(ForbiddenRegex),

    ModuleNode = elvis_code:find(#{
        of_types => [module],
        inside => root(RuleCfg)
    }),

    ModuleName =
        case ModuleNode of
            [Module] ->
                ktn_code:attr(value, Module);
            _ ->
                elvis_file:module(Target)
        end,
    ModuleNameStr = atom_to_list(ModuleName),

    case re_run(ModuleNameStr, RegexAllow) of
        nomatch ->
            [
                elvis_result:new_item(
                    "The name of this module is not acceptable by regular "
                    "expression '~s'",
                    [Regex]
                )
            ];
        {match, _} when RegexBlock =/= undefined ->
            case re_run(ModuleNameStr, RegexBlock) of
                % We check for forbidden names only after accepted names
                {match, _} ->
                    [
                        elvis_result:new_item(
                            "The name of this module name is forbidden by regular "
                            "expression '~s'",
                            [ForbiddenRegex]
                        )
                    ];
                nomatch ->
                    []
            end;
        _ ->
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
    SpecWithRecordNodes = elvis_code:find(#{
        of_types => [spec],
        inside => root(RuleCfg),
        filtered_by => fun spec_has_records/1
    }),

    [
        elvis_result:new_item(
            "an unexpected record was found in a spec; prefer creating a type for it and "
            "using that",
            #{node => SpecWithRecordNode}
        )
     || SpecWithRecordNode <- SpecWithRecordNodes
    ].

spec_has_records(SpecNode) ->
    elvis_code:find(#{
        of_types => [type],
        inside => SpecNode,
        filtered_by =>
            fun(TypeInSpecNode) ->
                ktn_code:attr(name, TypeInSpecNode) =:= record
            end,
        traverse => all
    }) =/= [].

dont_repeat_yourself(RuleCfg) ->
    MinComplexity = option(min_complexity, RuleCfg, ?FUNCTION_NAME),

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
    MaxLength = option(max_length, RuleCfg, ?FUNCTION_NAME),
    CountComments = option(count_comments, RuleCfg, ?FUNCTION_NAME),
    CountWhitespace = option(count_whitespace, RuleCfg, ?FUNCTION_NAME),
    CountDocs = option(count_docs, RuleCfg, ?FUNCTION_NAME),

    {Src0, _} = elvis_file:src(Target),
    DocParts = doc_bin_parts(Src0),
    Docs = iolist_to_binary(bin_parts_to_iolist(Src0, DocParts)),
    SrcParts = ignore_bin_parts(Src0, DocParts),
    Src = iolist_to_binary(bin_parts_to_iolist(Src0, SrcParts)),
    Lines0 = elvis_utils:split_all_lines(Src, [trim]),

    Lines = lists:filter(
        fun(Line) ->
            filter_comments_and_whitespace(Line, CountComments, CountWhitespace)
        end,
        Lines0
    ),

    DocLines =
        case CountDocs of
            true ->
                elvis_utils:split_all_lines(Docs, [trim]);
            false ->
                []
        end,

    ModLength = length(Lines) + length(DocLines),

    case ModLength > MaxLength of
        true ->
            [
                elvis_result:new_item(
                    "This module's lines-of-code count (~p) is higher than the configured limit",
                    [ModLength],
                    #{limit => MaxLength}
                )
            ];
        _ ->
            []
    end.

max_anonymous_function_arity(RuleCfg) ->
    MaxArity = option(max_arity, RuleCfg, ?FUNCTION_NAME),

    Funs = elvis_code:find(#{
        of_types => ['fun'],
        inside => root(RuleCfg),
        filtered_by =>
            fun(FunNode) ->
                %% Not having clauses means it's something like fun mod:f/10 and we don't want
                %% this rule to raise warnings for those. max_function_arity should take care of
                %% them.
                elvis_code:find(#{
                    of_types => [clause],
                    inside => FunNode
                }) =/= []
            end
    }),

    % We do this to recover the fun and arity
    FunArities = lists:filtermap(
        fun(Fun) ->
            [FirstClause | _] = elvis_code:find(#{
                of_types => [clause],
                inside => Fun
            }),
            Arity = length(ktn_code:node_attr(pattern, FirstClause)),
            case Arity > MaxArity of
                true ->
                    {true, {Fun, Arity}};
                false ->
                    false
            end
        end,
        Funs
    ),

    [
        elvis_result:new_item(
            "the arity (~p) of the anonymous function is higher than the "
            "configured limit",
            [Arity],
            #{node => Fun, limit => MaxArity}
        )
     || {Fun, Arity} <- FunArities
    ].

max_function_arity(RuleCfg) ->
    ExportedMaxArity = option(max_arity, RuleCfg, ?FUNCTION_NAME),
    NonExportedMaxArity0 = option(non_exported_max_arity, RuleCfg, ?FUNCTION_NAME),
    NonExportedMaxArity = specific_or_default(NonExportedMaxArity0, ExportedMaxArity),

    Root = root(RuleCfg),
    ExportedFunctions = exported_functions(Root),

    FunctionNodes0 = elvis_code:find(#{
        of_types => [function],
        inside => Root
    }),

    % We do this to recover the max arity (because it depends on "exported or not")
    FunctionNodeMaxArities = lists:filtermap(
        fun(FunctionNode) ->
            MaxArity =
                case is_exported_function(FunctionNode, ExportedFunctions) of
                    true ->
                        ExportedMaxArity;
                    false ->
                        NonExportedMaxArity
                end,
            case ktn_code:attr(arity, FunctionNode) > MaxArity of
                true ->
                    {true, {FunctionNode, MaxArity}};
                false ->
                    false
            end
        end,
        FunctionNodes0
    ),

    [
        elvis_result:new_item(
            "the arity of function '~p/~p' is higher than the configured limit",
            [ktn_code:attr(name, FunctionNode), ktn_code:attr(arity, FunctionNode)],
            #{node => FunctionNode, limit => MaxArity}
        )
     || {FunctionNode, MaxArity} <- FunctionNodeMaxArities
    ].

is_exported_function(FunctionNode, ExportedFunctions) ->
    Name = ktn_code:attr(name, FunctionNode),
    Arity = ktn_code:attr(arity, FunctionNode),
    lists:member({Name, Arity}, ExportedFunctions).

max_function_clause_length({_Config, Target, _RuleConfig} = RuleCfg) ->
    MaxLength = option(max_length, RuleCfg, ?FUNCTION_NAME),
    CountComments = option(count_comments, RuleCfg, ?FUNCTION_NAME),
    CountWhitespace = option(count_whitespace, RuleCfg, ?FUNCTION_NAME),

    {Src, _} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    {zippers, ClauseZippers} = elvis_code:find(#{
        of_types => [clause],
        inside => root(RuleCfg),
        filtered_by =>
            fun(ClauseZipper) ->
                is_function_clause(ClauseZipper, [function])
            end,
        filtered_from => zipper
    }),

    % We do this to recover the clause number and apply the configured filters
    {BigClauses, _} = lists:foldl(
        fun(ClauseZipper, {BigClauses0, ClauseNum}) ->
            ClauseNode = zipper:node(ClauseZipper),
            FilteredLines = filtered_lines_in(ClauseNode, Lines, CountComments, CountWhitespace),
            LineLen = length(FilteredLines),
            {
                case LineLen > MaxLength of
                    true ->
                        [{ClauseZipper, ClauseNum, LineLen} | BigClauses0];
                    false ->
                        BigClauses0
                end,
                ClauseNum + 1
            }
        end,
        {[], 1},
        ClauseZippers
    ),

    lists:map(
        fun({ClauseZipper, ClauseNum, LineLen}) ->
            FunctionNode = zipper:node(zipper:up(ClauseZipper)),

            elvis_result:new_item(
                "the code for the ~s clause of function '~p/~p' has ~p lines, which is higher than "
                "the configured limit",
                [
                    parse_clause_num(ClauseNum),
                    ktn_code:attr(name, FunctionNode),
                    ktn_code:attr(arity, FunctionNode),
                    LineLen
                ],
                #{node => FunctionNode, limit => MaxLength}
            )
        end,
        lists:reverse(BigClauses)
    ).

filtered_lines_in(Node, Lines, CountComments, CountWhitespace) ->
    {Min, Max} = node_line_limits(Node),
    NodeLines = lists:sublist(Lines, Min, Max - Min + 1),
    lists:filter(
        fun(NodeLine) ->
            filter_comments_and_whitespace(NodeLine, CountComments, CountWhitespace)
        end,
        NodeLines
    ).

filter_comments_and_whitespace(NodeLine, CountComments, CountWhitespace) ->
    (CountComments orelse not line_is_comment(NodeLine)) andalso
        (CountWhitespace orelse not line_is_whitespace(NodeLine)).

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
    MaxLength = option(max_length, RuleCfg, ?FUNCTION_NAME),
    CountComments = option(count_comments, RuleCfg, ?FUNCTION_NAME),
    CountWhitespace = option(count_whitespace, RuleCfg, ?FUNCTION_NAME),

    {Src, _} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    FunctionNodes = elvis_code:find(#{
        of_types => [function],
        inside => root(RuleCfg)
    }),

    % We do this to apply the configured filters
    BigFunctions = lists:filtermap(
        fun(FunctionNode) ->
            FilteredLines = filtered_lines_in(FunctionNode, Lines, CountComments, CountWhitespace),
            LineLen = length(FilteredLines),
            case LineLen > MaxLength of
                true ->
                    {true, {FunctionNode, LineLen}};
                false ->
                    false
            end
        end,
        FunctionNodes
    ),

    lists:map(
        fun({FunctionNode, LineLen}) ->
            elvis_result:new_item(
                "the code for function '~p/~p' has ~p lines, which is higher than the configured "
                "limit",
                [ktn_code:attr(name, FunctionNode), ktn_code:attr(arity, FunctionNode), LineLen],
                #{node => FunctionNode, limit => MaxLength}
            )
        end,
        BigFunctions
    ).

no_call(RuleCfg) ->
    DefaultFns = option(no_call_functions, RuleCfg, ?FUNCTION_NAME),
    Msg = "an unexpected call to '~p:~p/~p' was found (check no_call list)",
    no_call_common(RuleCfg, DefaultFns, Msg).

no_debug_call(RuleCfg) ->
    DefaultFns = option(debug_functions, RuleCfg, ?FUNCTION_NAME),
    Msg = "an unexpected debug call to '~p:~p/~p' was found",
    no_call_common(RuleCfg, DefaultFns, Msg).

no_common_caveats_call(RuleCfg) ->
    DefaultFns = option(caveat_functions, RuleCfg, ?FUNCTION_NAME),
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
    TryExprNodes = elvis_code:find(#{
        of_types => ['try'],
        inside => root(RuleCfg)
    }),

    InnerTryExprNodes = [
        TryExprContentNode
     || TryExprNode <- TryExprNodes,
        TryExprContentNode <- ktn_code:content(TryExprNode),
        ktn_code:type(TryExprContentNode) =:= 'try'
    ],

    [
        elvis_result:new_item(
            "an unexpected nested 'try...catch' expression was found",
            #{node => InnerTryExprNode}
        )
     || InnerTryExprNode <- InnerTryExprNodes
    ].

no_successive_maps(RuleCfg) ->
    MapExprNodes = elvis_code:find(#{
        of_types => [map],
        inside => root(RuleCfg),
        filtered_by =>
            fun(MapExprNode) ->
                InnerVar = ktn_code:node_attr(var, MapExprNode),
                ktn_code:type(InnerVar) =:= map
            end,
        traverse => content
    }),

    [
        elvis_result:new_item(
            "an unexpected map update after map construction/update was found",
            #{node => MapExprNode}
        )
     || MapExprNode <- MapExprNodes
    ].

atom_naming_convention(RuleCfg) ->
    Regex = option(regex, RuleCfg, ?FUNCTION_NAME),
    ForbiddenRegex = option(forbidden_regex, RuleCfg, ?FUNCTION_NAME),
    RegexEnclosed0 = option(enclosed_atoms, RuleCfg, ?FUNCTION_NAME),
    RegexEnclosed = specific_or_default(RegexEnclosed0, Regex),
    ForbiddenEnclosedRegex0 = option(forbidden_enclosed_regex, RuleCfg, ?FUNCTION_NAME),
    ForbiddenEnclosedRegex = specific_or_default(ForbiddenEnclosedRegex0, ForbiddenRegex),

    {zippers, AtomZippers} = elvis_code:find(#{
        of_types => [atom],
        inside => root(RuleCfg),
        filtered_by =>
            fun(AtomZipper) ->
                not check_parent_remote(AtomZipper)
            end,
        filtered_from => zipper,
        traverse => all
    }),

    lists:filtermap(
        fun(AtomZipper) ->
            AtomNode = zipper:node(AtomZipper),
            AtomName0 = ktn_code:attr(text, AtomNode),
            ValueAtomName = ktn_code:attr(value, AtomNode),
            {IsEnclosed, AtomName} = string_strip_enclosed(AtomName0),
            IsExceptionClass = is_exception_or_non_reversible(ValueAtomName),
            RegexAllow = re_compile_for_atom_type(IsEnclosed, Regex, RegexEnclosed),
            RegexBlock = re_compile_for_atom_type(
                IsEnclosed, ForbiddenRegex, ForbiddenEnclosedRegex
            ),
            AtomNameUnicode = unicode:characters_to_list(AtomName),

            case re_run(AtomNameUnicode, RegexAllow) of
                _ when IsExceptionClass, not IsEnclosed ->
                    false;
                nomatch when not IsEnclosed ->
                    {true,
                        elvis_result:new_item(
                            "the name of atom ~p is not acceptable by regular "
                            "expression '~s'",
                            [AtomName0, RegexAllow],
                            #{node => AtomNode}
                        )};
                nomatch when IsEnclosed ->
                    {true,
                        elvis_result:new_item(
                            "the name of enclosed atom ~p is not acceptable by regular "
                            "expression '~s'",
                            [AtomName0, RegexAllow],
                            #{node => AtomNode}
                        )};
                {match, _Captured} when RegexBlock =/= undefined ->
                    % We check for forbidden names only after accepted names
                    case re_run(AtomNameUnicode, RegexBlock) of
                        _ when IsExceptionClass, not IsEnclosed ->
                            false;
                        {match, _} when not IsEnclosed ->
                            {true,
                                elvis_result:new_item(
                                    "the name of atom ~p is forbidden by regular "
                                    "expression '~s'",
                                    [AtomName, RegexBlock],
                                    #{node => AtomNode}
                                )};
                        {match, _} when IsEnclosed ->
                            {true,
                                elvis_result:new_item(
                                    "the name of enclosed atom ~p is forbidden by regular "
                                    "expression '~s'",
                                    [AtomName, RegexBlock],
                                    #{node => AtomNode}
                                )};
                        _ ->
                            false
                    end;
                _ ->
                    false
            end
        end,
        AtomZippers
    ).

no_init_lists(RuleCfg) ->
    ConfigBehaviors = option(behaviours, RuleCfg, ?FUNCTION_NAME),

    Root = root(RuleCfg),

    InitClauseNodes =
        case is_relevant_behaviour(Root, ConfigBehaviors) of
            true ->
                FunctionNodes = elvis_code:find(#{
                    of_types => [function],
                    inside => Root,
                    filtered_by =>
                        fun(FunctionNode) ->
                            ktn_code:attr(name, FunctionNode) =:= init andalso
                                ktn_code:attr(arity, FunctionNode) =:= 1
                        end
                }),

                case FunctionNodes of
                    [] ->
                        [];
                    [Init1Fun] ->
                        Content = ktn_code:content(Init1Fun),
                        ListAttrClauses =
                            lists:filter(
                                fun(Clause) ->
                                    [Attribute] = ktn_code:node_attr(pattern, Clause),
                                    is_list_node(Attribute)
                                end,
                                Content
                            ),
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

    [
        elvis_result:new_item(
            "an avoidable list was found as argumeent to 'init' callback; prefer tuples, maps "
            "or records",
            #{node => InitClauseNode}
        )
     || InitClauseNode <- InitClauseNodes
    ].

is_relevant_behaviour(Root, ConfigBehaviors) ->
    Behaviours = elvis_code:find(#{of_types => [behaviour, behavior], inside => Root}),
    lists:any(
        fun(BehaviourNode) ->
            lists:member(
                ktn_code:attr(value, BehaviourNode), ConfigBehaviors
            )
        end,
        Behaviours
    ).

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

    case has_include_ms_transform(Root) of
        true ->
            [];
        false ->
            FunctionNodes = elvis_code:find(#{
                of_types => [call],
                inside => Root,
                filtered_by => fun is_ets_fun2ms/1
            }),

            [
                elvis_result:new_item(
                    "'ets:fun2ms/1' is used but the module is missing "
                    "'-include_lib(\"stdlib/include/ms_transform.hrl\").'",
                    #{node => FunctionNode}
                )
             || FunctionNode <- FunctionNodes
            ]
    end.

-spec is_ets_fun2ms(ktn_code:tree_node()) -> boolean().
is_ets_fun2ms(Node) ->
    Fun = ktn_code:node_attr(function, Node),
    Fun2 = ktn_code:node_attr(function, Fun),
    Module = ktn_code:node_attr(module, Fun),

    ktn_code:attr(value, Module) =:= ets andalso ktn_code:attr(value, Fun2) =:= fun2ms.

no_boolean_in_comparison(RuleCfg) ->
    OpNodes = elvis_code:find(#{
        of_types => [op],
        inside => root(RuleCfg),
        filtered_by =>
            fun(OpNode) ->
                lists:member(ktn_code:attr(operation, OpNode), ['==', '=:=', '/=', '=/=']) andalso
                    lists:any(
                        fun(OpContentNode) ->
                            is_boolean(ktn_code:attr(value, OpContentNode))
                        end,
                        ktn_code:content(OpNode)
                    )
            end,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "an avoidable comparison to boolean was found",
            #{node => OpNode}
        )
     || OpNode <- lists:uniq(OpNodes)
    ].

no_receive_without_timeout(RuleCfg) ->
    ReceiveExprNodes = elvis_code:find(#{
        of_types => ['receive'],
        inside => root(RuleCfg),
        filtered_by => fun is_receive_without_timeout/1
    }),

    [
        elvis_result:new_item(
            "a 'receive' expression was found without an 'after' clause; "
            "prefer to include 'after' in 'receive' expressions",
            #{node => ReceiveExprNode}
        )
     || ReceiveExprNode <- ReceiveExprNodes
    ].

is_receive_without_timeout(Receive) ->
    elvis_code:find(#{of_types => [receive_after], inside => Receive}) =:= [].

no_operation_on_same_value(RuleCfg) ->
    InterestingOps = option(operations, RuleCfg, ?FUNCTION_NAME),

    OpNodes = elvis_code:find(#{
        of_types => [op],
        inside => root(RuleCfg),
        filtered_by =>
            fun(OpNode) ->
                lists:member(ktn_code:attr(operation, OpNode), InterestingOps) andalso
                    same_value_on_both_sides(OpNode)
            end,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "redundant operation '~s' has the same value on both sides",
            [atom_to_list(ktn_code:attr(operation, OpNode))],
            #{node => OpNode}
        )
     || OpNode <- lists:uniq(OpNodes)
    ].

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
    elvis_code:find(#{
        of_types => [include_lib],
        inside => Root,
        filtered_by =>
            fun(IncludeLibNode) ->
                ktn_code:attr(value, IncludeLibNode) =:= "stdlib/include/ms_transform.hrl"
            end
    }) =/= [].

no_throw(RuleCfg) ->
    ThrowNodes = elvis_code:find(#{
        of_types => [call],
        inside => root(RuleCfg),
        filtered_by =>
            fun(OpNode) ->
                lists:any(
                    fun(MFA) ->
                        is_call(OpNode, MFA)
                    end,
                    [{throw, 1}, {erlang, throw, 1}]
                )
            end
    }),

    [
        elvis_result:new_item(
            "an avoidable call to 'throw/1' was found; prefer 'exit/1' or 'error/1'",
            #{node => ThrowNode}
        )
     || ThrowNode <- ThrowNodes
    ].

no_dollar_space(RuleCfg) ->
    CharNodes = elvis_code:find(#{
        of_types => [char],
        inside => root(RuleCfg),
        filtered_by =>
            fun(CharNode) ->
                ktn_code:attr(text, CharNode) =:= "$ "
            end,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "unexpected character '$ ' was found; prefer $\\s",
            #{node => CharNode}
        )
     || CharNode <- CharNodes
    ].

no_author(RuleCfg) ->
    AuthorNodes = elvis_code:find(#{
        of_types => [author],
        inside => root(RuleCfg)
    }),

    [
        elvis_result:new_item(
            "avoidable attribute '-author' was found",
            #{node => AuthorNode}
        )
     || AuthorNode <- AuthorNodes
    ].

no_import(RuleCfg) ->
    ImportNodes = elvis_code:find(#{
        of_types => [import],
        inside => root(RuleCfg)
    }),

    [
        elvis_result:new_item(
            "unexpected attribute '-import' was found",
            #{node => ImportNode}
        )
     || ImportNode <- ImportNodes
    ].

no_catch_expressions(RuleCfg) ->
    CatchExprNodes = elvis_code:find(#{
        of_types => ['catch'],
        inside => root(RuleCfg)
    }),

    [
        elvis_result:new_item(
            "an unexpected 'catch' expression was found; prefer a 'try' expression",
            #{node => CatchExprNode}
        )
     || CatchExprNode <- CatchExprNodes
    ].

no_single_clause_case(RuleCfg) ->
    CaseExprs = elvis_code:find(#{
        of_types => ['case'],
        inside => root(RuleCfg),
        filtered_by =>
            fun(CaseExpr) ->
                length(case_clauses_in(CaseExpr)) =:= 1
            end
    }),

    [
        elvis_result:new_item(
            "an avoidable single-clause 'case' expression was found",
            #{node => CaseExpr}
        )
     || CaseExpr <- CaseExprs
    ].

case_clauses_in(Node) ->
    [
        Clause
     || SubNode <- ktn_code:content(Node),
        ktn_code:type(SubNode) =:= case_clauses,
        Clause <- ktn_code:content(SubNode)
    ].

no_single_match_maybe(RuleCfg) ->
    MaybeNodes = elvis_code:find(#{
        of_types => ['maybe'],
        inside => root(RuleCfg),
        filtered_by =>
            fun(MaybeNode) ->
                length(ktn_code:content(MaybeNode)) =:= 1
            end
    }),

    [
        elvis_result:new_item(
            "an avoidable single-match 'maybe' block was found",
            #{node => MaybeNode}
        )
     || MaybeNode <- MaybeNodes
    ].

no_match_in_condition(RuleCfg) ->
    CaseExprNodes = elvis_code:find(#{
        of_types => [case_expr],
        inside => root(RuleCfg),
        filtered_by =>
            fun(CaseExprNode) ->
                %% case_expr followed by a match
                has_match_child(CaseExprNode) orelse
                    %% or case_expr followed by a block which contains a match in the first layer
                    lists:any(
                        fun(CaseExprContent) ->
                            ktn_code:type(CaseExprContent) =:= block andalso
                                has_match_child(CaseExprContent)
                        end,
                        ktn_code:content(CaseExprNode)
                    )
            end
    }),

    [
        elvis_result:new_item(
            "an avoidable match condition in a 'case' expression was found; prefer matching "
            "in 'case' clauses",
            #{node => CaseExprNode}
        )
     || CaseExprNode <- CaseExprNodes
    ].

is_match(Node) ->
    lists:member(ktn_code:type(Node), match_operators()).

has_match_child(Node) ->
    lists:any(fun is_match/1, ktn_code:content(Node)).

numeric_format(RuleCfg) ->
    Root = root(RuleCfg),
    Regex = option(regex, RuleCfg, ?FUNCTION_NAME),
    IntRegex0 = option(int_regex, RuleCfg, ?FUNCTION_NAME),
    IntRegex = specific_or_default(IntRegex0, Regex),
    FloatRegex0 = option(float_regex, RuleCfg, ?FUNCTION_NAME),
    FloatRegex = specific_or_default(FloatRegex0, Regex),
    IntNodes = elvis_code:find(#{of_types => [integer], inside => Root}),
    FloatNodes = elvis_code:find(#{of_types => [float], inside => Root}),
    check_numeric_format(
        IntRegex,
        IntNodes,
        check_numeric_format(FloatRegex, FloatNodes, [])
    ).

behaviour_spelling(RuleCfg) ->
    Spelling = option(spelling, RuleCfg, ?FUNCTION_NAME),

    BehaviourNodes = elvis_code:find(#{
        of_types => [behaviour, behavior],
        inside => root(RuleCfg),
        filtered_by =>
            fun(BehaviourNode) ->
                ktn_code:type(BehaviourNode) =/= Spelling
            end
    }),

    [
        elvis_result:new_item(
            "an unexpected spelling of 'behavio[u]r' was found; prefer ~p",
            [Spelling],
            #{node => BehaviourNode}
        )
     || BehaviourNode <- BehaviourNodes
    ].

param_pattern_matching(RuleCfg) ->
    Side = option(side, RuleCfg, ?FUNCTION_NAME),

    {zippers, ClauseZippers} = elvis_code:find(#{
        of_types => [clause],
        inside => root(RuleCfg),
        filtered_by =>
            fun(ClauseZipper) ->
                is_function_clause(ClauseZipper, [function, 'fun'])
            end,
        filtered_from => zipper,
        traverse => all
    }),

    MatchesInFunctionClauses =
        lists:append(
            lists:map(
                fun(ClauseZipper) ->
                    ClauseNode = zipper:node(ClauseZipper),
                    ClausePatterns = ktn_code:node_attr(pattern, ClauseNode),
                    lists:filter(
                        fun(ClausePattern) ->
                            ktn_code:type(ClausePattern) =:= match
                        end,
                        ClausePatterns
                    )
                end,
                ClauseZippers
            )
        ),

    MatchVars = lists:filtermap(
        fun(Match) ->
            case lists:map(fun ktn_code:type/1, ktn_code:content(Match)) of
                [var, var] ->
                    false;
                [var, _] when Side =:= right ->
                    [Var, _] = ktn_code:content(Match),
                    {true, {Match, Var}};
                [_, var] when Side =:= left ->
                    [_, Var] = ktn_code:content(Match),
                    {true, {Match, Var}};
                _ ->
                    false
            end
        end,
        MatchesInFunctionClauses
    ),

    [
        elvis_result:new_item(
            "variable '~s' is used to match an argument, but placed on "
            "the wrong side of it; prefer the ~p side",
            [atom_to_list(ktn_code:attr(name, Var)), Side],
            #{node => Match}
        )
     || {Match, Var} <- MatchVars
    ].

is_function_clause(ClauseZipper, ParentNodeTypes) ->
    ClauseParent = zipper:up(ClauseZipper),
    ParentNode = zipper:node(ClauseParent),
    ParentNodeType = ktn_code:type(ParentNode),
    lists:member(ParentNodeType, ParentNodeTypes).

consistent_generic_type(RuleCfg) ->
    PreferredType = option(preferred_type, RuleCfg, ?FUNCTION_NAME),

    TypeNodes = elvis_code:find(#{
        of_types => [type, callback],
        inside => root(RuleCfg),
        filtered_by =>
            fun(TypeNode) ->
                TypeNodeName = ktn_code:attr(name, TypeNode),
                lists:member(TypeNodeName, [term, any]) andalso TypeNodeName =/= PreferredType
            end,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "unexpected type '~p/0' was found; prefer ~p/0",
            [ktn_code:attr(name, TypeNode), PreferredType],
            #{node => TypeNode}
        )
     || TypeNode <- TypeNodes
    ].

always_shortcircuit(RuleCfg) ->
    Operators = #{'and' => 'andalso', 'or' => 'orelse'},

    OpNodes = elvis_code:find(#{
        of_types => [op],
        inside => root(RuleCfg),
        filtered_by =>
            fun(OpNode) ->
                NodeOperation = ktn_code:attr(operation, OpNode),
                lists:member(NodeOperation, maps:keys(Operators))
            end,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "unexpected non-shortcircuiting operator '~p' was found; prefer ~p",
            [
                ktn_code:attr(operation, OpNode),
                maps:get(ktn_code:attr(operation, OpNode), Operators)
            ],
            #{node => OpNode}
        )
     || OpNode <- OpNodes
    ].

export_used_types(RuleCfg) ->
    Root = root(RuleCfg),

    case is_otp_behaviour(Root) of
        false ->
            ExportedFunctions = exported_functions(Root),
            ExportedTypes = exported_types(Root),

            SpecNodes = elvis_code:find(#{
                of_types => [spec],
                inside => Root,
                filtered_by =>
                    fun(SpecNode) ->
                        is_exported_function(SpecNode, ExportedFunctions)
                    end
            }),

            UsedTypes =
                lists:usort(
                    lists:flatmap(
                        fun(SpecNode) ->
                            UserTypeNodes = elvis_code:find(#{
                                of_types => [user_type],
                                inside => SpecNode,
                                traverse => all
                            }),
                            % yes, on a -type line, the arity is based on `args`, but on
                            % a -spec line, it's based on `content`
                            [
                                {Name, length(Vars)}
                             || #{attrs := #{name := Name}, content := Vars} <- UserTypeNodes
                            ]
                        end,
                        SpecNodes
                    )
                ),
            UnexportedUsedTypes = lists:subtract(UsedTypes, ExportedTypes),

            Locations = map_type_declarations_to_location(Root),

            lists:map(
                fun({Name, Arity}) ->
                    {Line, Column} = maps:get({Name, Arity}, Locations, {-1, -1}),
                    elvis_result:new_item(
                        "type '~p/~p' is used by an exported function; prefer to also export the "
                        "type",
                        [Name, Arity],
                        #{line => Line, column => Column}
                    )
                end,
                UnexportedUsedTypes
            );
        true ->
            []
    end.

private_data_types(RuleCfg) ->
    TypesToCheck = option(apply_to, RuleCfg, ?FUNCTION_NAME),

    Root = root(RuleCfg),
    ExportedTypes = exported_types(Root),
    PublicDataTypes = public_data_types(TypesToCheck, Root, ExportedTypes),
    Locations = map_type_declarations_to_location(Root),

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

public_data_types(TypesToCheck, Root, ExportedTypes) ->
    TypeAttrNodes = elvis_code:find(#{
        of_types => [type_attr],
        inside => Root,
        filtered_by =>
            fun(TypeAttrNode) ->
                TypeAttrNodeType = ktn_code:node_attr(type, TypeAttrNode),
                TypeAttrNodeType =/= undefined andalso
                    lists:member(ktn_code:attr(name, TypeAttrNodeType), TypesToCheck)
            end,
        traverse => all
    }),

    NameArities = lists:map(
        fun(TypeAttrNode) ->
            #{
                attrs := #{
                    name := Name
                },
                node_attrs := #{
                    args := Args
                }
            } = TypeAttrNode,
            {Name, length(Args)}
        end,
        TypeAttrNodes
    ),

    lists:filter(
        fun({Name, Arity}) ->
            lists:member({Name, Arity}, ExportedTypes)
        end,
        NameArities
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map_type_declarations_to_location(ktn_code:tree_node()) ->
    #{{atom(), number()} => number()}.
map_type_declarations_to_location(Root) ->
    AllTypes = elvis_code:find(#{of_types => [type_attr], inside => Root}),
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
                case re_run(Number, Regex) of
                    nomatch ->
                        [
                            elvis_result:new_item(
                                "the format of number '~s' is not acceptable by regular "
                                "expression '~s'",
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
    re_compile(Regex);
re_compile_for_atom_type(true = _IsEnclosed, _Regex, RegexEnclosed) ->
    re_compile(RegexEnclosed).

-spec line_is_comment(binary()) -> boolean().
line_is_comment(Line) ->
    case re_run(Line, "^[ \t]*%") of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

-spec line_is_whitespace(binary()) -> boolean().
line_is_whitespace(Line) ->
    case re_run(Line, "^[ \t]*$") of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

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

-spec generate_space_check_results(
    {Lines :: [binary()], Encoding :: latin1 | utf8},
    Nodes :: [ktn_code:tree_node()],
    Rule :: {right | left, string()},
    How :: {should_have, []} | {should_not_have, [{string(), {ok, re:mp()}}]}
) ->
    [elvis_result:item()].
generate_space_check_results(
    {Lines, Encoding},
    UnfilteredNodes,
    {Position, Text},
    {How0, _} = How
) ->
    Nodes = lists:filter(
        fun(Node) ->
            ktn_code:attr(text, Node) =:= Text orelse
                (ktn_code:type(Node) =:= dot andalso Text =:= ".")
        end,
        UnfilteredNodes
    ),

    lists:filtermap(
        fun(Node) ->
            Location = ktn_code:attr(location, Node),
            case character_at_location(Position, Lines, Text, Location, Encoding, How) of
                Char when Char =:= $\s, How0 =:= should_have ->
                    false;
                Char when Char =/= $\s, How0 =:= should_not_have ->
                    false;
                _ when How0 =:= should_have ->
                    {true,
                        elvis_result:new_item(
                            "there is a missing space to the ~p of '~s'",
                            [Position, Text],
                            #{node => Node}
                        )};
                _ when How0 =:= should_not_have ->
                    {true,
                        elvis_result:new_item(
                            "an unexpected space was found to the ~p of '~s'",
                            [Position, Text],
                            #{node => Node}
                        )}
            end
        end,
        Nodes
    ).

maybe_re_run(_Line, undefined = _Regex) ->
    nomatch;
maybe_re_run(Line, Regex) ->
    re_run(Line, Regex).

-spec character_at_location(
    Position :: atom(),
    Lines :: [binary()],
    Text :: string(),
    Location :: {integer(), integer()},
    Encoding :: latin1 | utf8,
    How :: {should_have, []} | {should_not_have, [{string(), {ok, re:mp()}}]}
) ->
    char().
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

%% @doc Takes a node and returns all nodes where the nesting limit is exceeded.
-spec past_nesting_limit(ktn_code:tree_node(), integer()) ->
    [{ktn_code:tree_node(), integer()}].
past_nesting_limit(Node, MaxLevel) ->
    past_nesting_limit(Node, 0, MaxLevel).

past_nesting_limit(Node, CurrentLevel, MaxLevel) when CurrentLevel > MaxLevel ->
    [{Node, CurrentLevel}];
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

-spec is_var(zipper:zipper(_)) -> boolean().
is_var(Zipper) ->
    PrevLocation =
        case ktn_code:attr(location, zipper:node(Zipper)) of
            {L, 1} ->
                {L - 1, 9999};
            {L, C} ->
                {L, C - 1}
        end,
    case
        find_token(
            zipper:root(Zipper), PrevLocation
        )
    of
        not_found ->
            true;
        {ok, PrevToken} ->
            ktn_code:type(PrevToken) =/= '?'
    end.

find_token(Root, Location) ->
    Fun = fun(Token) -> is_at_location(Token, Location) end,
    Tokens = ktn_code:attr(tokens, Root),
    case lists:filter(Fun, Tokens) of
        [] ->
            not_found;
        [Token | _] ->
            {ok, Token}
    end.

is_at_location(#{attrs := #{location := {Line, NodeCol}}} = Node, {Line, Column}) ->
    Text = ktn_code:attr(text, Node),
    Length = length(Text),
    NodeCol =< Column andalso Column < NodeCol + Length;
is_at_location(_, _) ->
    false.

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

-spec is_otp_behaviour(ktn_code:tree_node()) -> boolean().
is_otp_behaviour(Root) ->
    OtpSet = sets:from_list([gen_server, gen_event, gen_fsm, gen_statem, supervisor_bridge]),
    case elvis_code:find(#{of_types => [behaviour, behavior], inside => Root}) of
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
    elvis_code:find(#{
        of_types => [record_attr],
        inside => Root,
        filtered_by =>
            fun(RecordAttrNode) ->
                ktn_code:attr(name, RecordAttrNode) =:= state
            end
    }) =/= [].

-spec has_state_type(ktn_code:tree_node()) -> boolean().
has_state_type(Root) ->
    elvis_code:find(#{
        of_types => [type_attr, opaque],
        inside => Root,
        filtered_by =>
            fun(TypeAttrOrOpaqueNode) ->
                case ktn_code:type(TypeAttrOrOpaqueNode) of
                    type_attr ->
                        ktn_code:attr(name, TypeAttrOrOpaqueNode) =:= state;
                    opaque ->
                        case ktn_code:attr(value, TypeAttrOrOpaqueNode) of
                            {state, _, _} ->
                                true;
                            _ ->
                                false
                        end
                end
            end
    }) =/= [].

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

no_call_common(RuleCfg, NoCallFuns, Msg) ->
    Root = root(RuleCfg),

    Calls = elvis_code:find(#{of_types => [call], inside => Root}),
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
    case re_run(Src, RE, [global, {capture, first, index}, unicode]) of
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

re_run(Subject, RE) ->
    re_run(Subject, RE, []).

re_run(Subject, RE, Options) ->
    re:run(Subject, RE, Options).

re_compile(Regexp) ->
    re_compile(Regexp, [unicode]).

re_compile(undefined, _Options) ->
    undefined;
re_compile(Regexp, Options) ->
    {ok, MP} = re:compile(Regexp, Options),
    MP.
