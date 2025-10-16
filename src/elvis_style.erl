-module(elvis_style).

-behaviour(elvis_rule).
-export([default/1]).

-export([
    function_naming_convention/2,
    variable_naming_convention/2,
    variable_casing/2,
    macro_naming_convention/2,
    no_macros/2,
    no_specs/2,
    no_types/2,
    no_includes/2,
    no_block_expressions/2,
    operator_spaces/2,
    no_space/2,
    no_space_after_pound/2,
    no_deep_nesting/2,
    no_god_modules/2,
    no_if_expression/2,
    no_invalid_dynamic_calls/2,
    no_used_ignored_variables/2,
    no_behavior_info/2,
    module_naming_convention/2,
    state_record_and_type/2,
    no_spec_with_records/2,
    dont_repeat_yourself/2,
    max_module_length/2,
    max_anonymous_function_arity/2,
    max_function_arity/2,
    max_anonymous_function_length/2,
    max_anonymous_function_clause_length/2,
    max_function_length/2,
    max_function_clause_length/2,
    max_record_fields/2,
    max_map_type_keys/2,
    no_call/2,
    no_debug_call/2,
    no_common_caveats_call/2,
    no_nested_try_catch/2,
    no_successive_maps/2,
    atom_naming_convention/2,
    no_throw/2,
    no_dollar_space/2,
    no_author/2,
    no_import/2,
    no_catch_expressions/2,
    no_single_clause_case/2,
    no_single_match_maybe/2,
    numeric_format/2,
    behaviour_spelling/2,
    always_shortcircuit/2,
    generic_type/2,
    export_used_types/2,
    no_match_in_condition/2,
    param_pattern_matching/2,
    private_data_types/2,
    no_init_lists/2,
    ms_transform_included/2,
    no_boolean_in_comparison/2,
    no_operation_on_same_value/2,
    no_receive_without_timeout/2,
    prefer_unquoted_atoms/2,
    guard_operators/2,
    simplify_anonymous_functions/2,
    prefer_include/2,
    prefer_strict_generators/2,
    strict_term_equivalence/2,
    macro_definition_parentheses/2
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(RuleName :: atom()) -> elvis_rule:def().
default(no_init_lists) ->
    elvis_rule:defmap(#{
        behaviours => [gen_server, gen_statem, gen_fsm, supervisor, supervisor_bridge, gen_event]
    });
default(macro_naming_convention) ->
    elvis_rule:defmap(#{
        regex => "^[A-Z](_?[A-Z0-9]+)*$",
        forbidden_regex => undefined
    });
default(operator_spaces) ->
    elvis_rule:defmap(#{
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
    });
default(no_space) ->
    % ) can happen at the start of lines; all others can't
    elvis_rule:defmap(#{
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
    });
default(no_deep_nesting) ->
    elvis_rule:defmap(#{
        level => 4
    });
default(no_god_modules) ->
    elvis_rule:defmap(#{
        limit => 25
    });
default(function_naming_convention) ->
    elvis_rule:defmap(#{
        regex => "^[a-z](_?[a-z0-9]+)*(_test_)?$",
        forbidden_regex => undefined
    });
default(variable_naming_convention) ->
    elvis_rule:defmap(#{
        regex => "^_?([A-Z][0-9a-zA-Z]*)$",
        forbidden_regex => undefined
    });
default(module_naming_convention) ->
    elvis_rule:defmap(#{
        regex => "^[a-z](_?[a-z0-9]+)*(_SUITE)?$",
        forbidden_regex => undefined
    });
default(dont_repeat_yourself) ->
    elvis_rule:defmap(#{
        min_complexity => 10
    });
default(max_module_length) ->
    elvis_rule:defmap(#{
        max_length => 500,
        count_comments => false,
        count_whitespace => false,
        count_docs => false
    });
default(max_anonymous_function_arity) ->
    elvis_rule:defmap(#{
        max_arity => 5
    });
default(max_function_arity) ->
    elvis_rule:defmap(#{
        max_arity => 8,
        non_exported_max_arity => 10
    });
default(max_anonymous_function_length) ->
    elvis_rule:defmap(#{
        max_length => 30,
        count_comments => false,
        count_whitespace => false
    });
default(max_anonymous_function_clause_length) ->
    elvis_rule:defmap(#{
        max_length => 30,
        count_comments => false,
        count_whitespace => false
    });
default(max_function_length) ->
    elvis_rule:defmap(#{
        max_length => 30,
        count_comments => false,
        count_whitespace => false
    });
default(max_function_clause_length) ->
    elvis_rule:defmap(#{
        max_length => 30,
        count_comments => false,
        count_whitespace => false
    });
default(max_record_fields) ->
    elvis_rule:defmap(#{
        max_fields => 25
    });
default(max_map_type_keys) ->
    elvis_rule:defmap(#{
        max_keys => 25
    });
default(no_call) ->
    elvis_rule:defmap(#{
        no_call_functions => []
    });
default(no_debug_call) ->
    elvis_rule:defmap(#{
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
    });
default(no_common_caveats_call) ->
    elvis_rule:defmap(#{
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
    });
default(atom_naming_convention) ->
    elvis_rule:defmap(#{
        regex => "^[a-z](_?[a-z0-9]+)*(_SUITE)?$",
        enclosed_atoms => ".*",
        forbidden_regex => undefined,
        forbidden_enclosed_regex => undefined
    });
default(numeric_format) ->
    elvis_rule:defmap(#{
        % Not restrictive. Those who want more restrictions can set it like "^[^_]*$"
        regex => ".*",
        int_regex => same,
        float_regex => same
    });
default(guard_operators) ->
    elvis_rule:defmap(#{
        preferred_syntax => per_expression
    });
default(behaviour_spelling) ->
    elvis_rule:defmap(#{
        spelling => behaviour
    });
default(param_pattern_matching) ->
    elvis_rule:defmap(#{
        side => right
    });
default(generic_type) ->
    elvis_rule:defmap(#{
        preferred_type => term
    });
default(private_data_types) ->
    elvis_rule:defmap(#{
        apply_to => [record, map, tuple]
    });
default(no_operation_on_same_value) ->
    elvis_rule:defmap(#{
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
    });
default(no_macros) ->
    elvis_rule:defmap(#{
        allow => []
    });
default(_RuleName) ->
    elvis_rule:defmap(#{}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type binary_part() :: {Start :: non_neg_integer(), Length :: integer()}.

-spec function_naming_convention(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
function_naming_convention(Rule, ElvisConfig) ->
    Regex = elvis_rule:option(regex, Rule),
    ForbiddenRegex = elvis_rule:option(forbidden_regex, Rule),

    {nodes, FunctionNodes} = elvis_code:find(#{
        of_types => [function],
        inside => elvis_code:root(Rule, ElvisConfig)
    }),

    lists:filtermap(
        fun(FunctionNode) ->
            FunctionName = function_name(FunctionNode),

            case re_run(FunctionName, Regex) of
                nomatch ->
                    {true,
                        elvis_result:new_item(
                            "the name of function ~p is not acceptable by regular "
                            "expression '~s'",
                            [FunctionName, Regex],
                            #{node => FunctionNode}
                        )};
                {match, _} when ForbiddenRegex =/= undefined ->
                    % We check for forbidden names only after accepted names
                    case re_run(FunctionName, ForbiddenRegex) of
                        {match, _} ->
                            {true,
                                elvis_result:new_item(
                                    "the name of function ~p is forbidden by regular "
                                    "expression '~s'",
                                    [FunctionName, ForbiddenRegex],
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

function_name(FunctionNode) ->
    FunctionName = ktn_code:attr(name, FunctionNode),
    unicode:characters_to_list(atom_to_list(FunctionName)).

-spec variable_casing(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
variable_casing(Rule, ElvisConfig) ->
    Root = elvis_code:root(Rule, ElvisConfig),

    {zippers, VarZippers} = elvis_code:find(#{
        of_types => [var],
        inside => Root,
        filtered_by => fun is_var/1,
        filtered_from => zipper,
        traverse => all
    }),

    GroupedZippers = maps:groups_from_list(fun canonical_variable_name_up/1, VarZippers),

    lists:filtermap(
        fun({_CanonicalVariableName, [FirstVarZipper | OtherVarZippers]}) ->
            FirstName = canonical_variable_name(FirstVarZipper),
            OtherNames = unique_other_names(OtherVarZippers, FirstName),

            case OtherNames of
                [] ->
                    false;
                _ ->
                    {true,
                        elvis_result:new_item(
                            "variable '~s' (first used in line ~p) is written in "
                            "different ways within the module: ~p",
                            [FirstName, line(zipper:node(FirstVarZipper)), OtherNames],
                            #{zipper => FirstVarZipper}
                        )}
            end
        end,
        maps:to_list(GroupedZippers)
    ).

unique_other_names(OtherVarZippers, FirstName) ->
    lists:usort([
        OtherName
     || OtherVarZipper <- OtherVarZippers,
        (OtherName = canonical_variable_name(OtherVarZipper)) =/= FirstName
    ]).

canonical_variable_name(VarZipper) ->
    VarNode = zipper:node(VarZipper),

    case atom_to_list(ktn_code:attr(name, VarNode)) of
        [$_ | Rest] ->
            Rest;
        VarNameStr ->
            VarNameStr
    end.

canonical_variable_name_up(VarZipper) ->
    string:uppercase(canonical_variable_name(VarZipper)).

-spec variable_naming_convention(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
variable_naming_convention(Rule, ElvisConfig) ->
    Regex = elvis_rule:option(regex, Rule),
    ForbiddenRegex = elvis_rule:option(forbidden_regex, Rule),

    {zippers, VarZippers} = elvis_code:find(#{
        of_types => [var],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_var/1,
        filtered_from => zipper,
        traverse => all
    }),

    lists:filtermap(
        fun(VarZipper) ->
            VariableName = variable_name(VarZipper),

            case re_run(VariableName, Regex) of
                nomatch when VariableName =/= "_" ->
                    {true,
                        elvis_result:new_item(
                            "the name of variable ~p is not acceptable by regular "
                            "expression '~s'",
                            [VariableName, Regex],
                            #{zipper => VarZipper}
                        )};
                {match, _} when ForbiddenRegex =/= undefined ->
                    % We check for forbidden names only after accepted names
                    case re_run(VariableName, ForbiddenRegex) of
                        {match, _} ->
                            {true,
                                elvis_result:new_item(
                                    "the name of variable ~p is forbidden by regular "
                                    "expression '~s'",
                                    [VariableName, ForbiddenRegex],
                                    #{zipper => VarZipper}
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

variable_name(VarZipper) ->
    VarNode = zipper:node(VarZipper),
    atom_to_list(ktn_code:attr(name, VarNode)).

-spec macro_naming_convention(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
macro_naming_convention(Rule, ElvisConfig) ->
    Regex = elvis_rule:option(regex, Rule),
    ForbiddenRegex = elvis_rule:option(forbidden_regex, Rule),

    RegexAllow = re_compile(Regex),
    RegexBlock = re_compile(ForbiddenRegex),

    {nodes, MacroNodes} = elvis_code:find(#{
        of_types => [define],
        inside => elvis_code:root(Rule, ElvisConfig),
        traverse => all
    }),

    lists:filtermap(
        fun(MacroNode) ->
            MacroName = stripped_macro_name_from(MacroNode),

            case re_run(MacroName, RegexAllow) of
                nomatch ->
                    {true,
                        elvis_result:new_item(
                            "the name of macro ~p is not acceptable by "
                            "regular expression '~s'",
                            [original_macro_name_from(MacroNode), Regex],
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
                                    [original_macro_name_from(MacroNode), ForbiddenRegex],
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

-spec no_macros(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_macros(Rule, ElvisConfig) ->
    AllowedMacros = elvis_rule:option(allow, Rule) ++ eep_predef_macros() ++ logger_macros(),

    {nodes, MacroNodes} = elvis_code:find(#{
        of_types => [macro],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(MacroNode) ->
                is_allowed_macro(MacroNode, AllowedMacros)
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

is_allowed_macro(MacroNode, AllowedMacros) ->
    Macro = list_to_atom(ktn_code:attr(name, MacroNode)),
    not lists:member(Macro, AllowedMacros).

-spec no_types(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_types(Rule, ElvisConfig) ->
    {nodes, TypeAttrNodes} = elvis_code:find(#{
        of_types => [type_attr],
        inside => elvis_code:root(Rule, ElvisConfig)
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

-spec no_includes(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_includes(Rule, ElvisConfig) ->
    {nodes, IncludeNodes} = elvis_code:find(#{
        of_types => [include, include_lib],
        inside => elvis_code:root(Rule, ElvisConfig)
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

-spec no_specs(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_specs(Rule, ElvisConfig) ->
    {nodes, SpecNodes} = elvis_code:find(#{
        of_types => [spec],
        inside => elvis_code:root(Rule, ElvisConfig)
    }),

    [
        elvis_result:new_item(
            "an unexpected spec for was found function '~p'; avoid specs in .hrl files",
            [ktn_code:attr(name, SpecNode)],
            #{node => SpecNode}
        )
     || SpecNode <- SpecNodes
    ].

-spec no_block_expressions(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_block_expressions(Rule, ElvisConfig) ->
    {nodes, BlockExprs} = elvis_code:find(#{
        of_types => ['begin'],
        inside => tokens_as_content(elvis_code:root(Rule, ElvisConfig)),
        traverse => all
    }),

    [
        elvis_result:new_item(
            "an avoidable block expression ('begin...end') was found",
            [],
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

-spec no_space_after_pound(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_space_after_pound(Rule, ElvisConfig) ->
    {nodes, TextNodes} = elvis_code:find(#{
        of_types => undefined,
        inside => tokens_as_content(elvis_code:root(Rule, ElvisConfig)),
        filtered_by => fun is_text_node/1
    }),

    {Lines, Encoding} = lines_in(elvis_rule:file(Rule)),
    generate_space_check_results({Lines, Encoding}, TextNodes, {right, "#"}, {should_not_have, []}).

lines_in(File) ->
    {Src, File1} = elvis_file:src(File),
    Encoding = elvis_file:encoding(File1),
    {elvis_utils:split_all_lines(Src), Encoding}.

-spec operator_spaces(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
operator_spaces(Rule, ElvisConfig) ->
    OptRules = elvis_rule:option(rules, Rule),

    Root = elvis_code:root(Rule, ElvisConfig),

    {nodes, OpNodes} = elvis_code:find(#{
        of_types => undefined,
        inside => Root,
        filtered_by => fun is_operator_node/1
    }),

    {nodes, PunctuationTokens} = elvis_code:find(#{
        of_types => ['=', '&&', ',', ';', dot, '->', ':', '::', '|', '||'],
        inside => tokens_as_content(Root)
    }),

    AllNodes = OpNodes ++ PunctuationTokens,

    {Lines, Encoding} = lines_in(elvis_rule:file(Rule)),

    lists:flatmap(
        fun(OptRule) ->
            generate_space_check_results({Lines, Encoding}, AllNodes, OptRule, {should_have, []})
        end,
        OptRules
    ).

%% @doc Returns true when the node is an operator with more than one operand
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
    NodeOperators = ktn_code:content(Node),
    IsNotSingleNodeOperator = length(NodeOperators) > 1 andalso lists:member(NodeType, OpOrMatch),
    IsNotSingleNodeOperator orelse lists:member(NodeType, ExtraOpsTypes).

match_operators() ->
    [match, maybe_match].

-spec no_space(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_space(Rule, ElvisConfig) ->
    OptRules = elvis_rule:option(rules, Rule),

    {nodes, TextNodes} = elvis_code:find(#{
        of_types => undefined,
        inside => tokens_as_content(elvis_code:root(Rule, ElvisConfig)),
        filtered_by => fun is_text_node/1
    }),

    AllSpaceUntilText = [
        {Text, re_compile("^[ ]+" ++ escape_regex(Text))}
     || {left, Text} <- OptRules
    ],

    {Lines, Encoding} = lines_in(elvis_rule:file(Rule)),

    lists:flatmap(
        fun(OptRule) ->
            generate_space_check_results(
                {Lines, Encoding}, TextNodes, OptRule, {should_not_have, AllSpaceUntilText}
            )
        end,
        OptRules
    ).

escape_regex(Text) ->
    EscapePattern = "(\\.|\\[|\\]|\\^|\\$|\\+|\\*|\\?|\\{|\\}|\\(|\\)|\\||\\\\)",
    re:replace(Text, EscapePattern, "\\\\\\1", [{return, list}, global]).

is_text_node(Node) ->
    ktn_code:attr(text, Node) =/= "".

-spec no_deep_nesting(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_deep_nesting(Rule, ElvisConfig) ->
    MaxLevel = elvis_rule:option(level, Rule),

    {nodes, ParentNodes} = elvis_code:find(#{
        of_types => undefined,
        inside => elvis_code:root(Rule, ElvisConfig)
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

-spec no_god_modules(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_god_modules(Rule, ElvisConfig) ->
    Limit = elvis_rule:option(limit, Rule),

    Root = elvis_code:root(Rule, ElvisConfig),
    ExportedFunctions = exported_functions(Root),
    Count = length(ExportedFunctions),

    case Count > Limit of
        true ->
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
    {nodes, ExportNodes} = elvis_code:find(#{
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
    {nodes, ExportNodes} = elvis_code:find(#{
        of_types => [export_type],
        inside => Root
    }),
    lists:flatmap(
        fun(Node) ->
            ktn_code:attr(value, Node)
        end,
        ExportNodes
    ).

-spec no_if_expression(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_if_expression(Rule, ElvisConfig) ->
    {nodes, IfExprNodes} = elvis_code:find(#{
        of_types => ['if'],
        inside => elvis_code:root(Rule, ElvisConfig)
    }),

    [
        elvis_result:new_item(
            "an unexpected 'if' expression was found",
            [],
            #{node => IfExprNode}
        )
     || IfExprNode <- IfExprNodes
    ].

-spec no_invalid_dynamic_calls(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_invalid_dynamic_calls(Rule, ElvisConfig) ->
    Root = elvis_code:root(Rule, ElvisConfig),

    InvalidCallNodes =
        case has_callbacks(Root) of
            true ->
                [];
            false ->
                {nodes, InvalidCallNodes0} = elvis_code:find(#{
                    of_types => [call],
                    inside => Root,
                    filtered_by => fun is_dynamic_call/1,
                    traverse => all
                }),
                InvalidCallNodes0
        end,

    [
        elvis_result:new_item(
            "an unexpected dynamic function call was found; prefer "
            "making dynamic calls only in modules that define callbacks",
            [],
            #{node => InvalidCallNode}
        )
     || InvalidCallNode <- InvalidCallNodes
    ].

has_callbacks(Root) ->
    {nodes, Nodes} = elvis_code:find(#{
        of_types => [callback],
        inside => Root
    }),
    Nodes =/= [].

-spec no_used_ignored_variables(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_used_ignored_variables(Rule, ElvisConfig) ->
    {zippers, IgnoredVarZippers} = elvis_code:find(#{
        of_types => [var],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_ignored_var/1,
        filtered_from => zipper
    }),

    [
        elvis_result:new_item(
            "an unexpected use of an ignored variable was found",
            [],
            #{zipper => IgnoredVarZipper}
        )
     || IgnoredVarZipper <- IgnoredVarZippers
    ].

-spec no_behavior_info(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_behavior_info(Rule, ElvisConfig) ->
    {nodes, BehaviourInfoNodes} = elvis_code:find(#{
        of_types => [function],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun has_behavior_info/1
    }),

    [
        elvis_result:new_item(
            "an avoidable 'behavio[u]r_info/1' declaration was found; prefer '-callback' "
            "attributes",
            [],
            #{node => BehaviourInfoNode}
        )
     || BehaviourInfoNode <- BehaviourInfoNodes
    ].

has_behavior_info(FunctionNode) ->
    FunctionName = ktn_code:attr(name, FunctionNode),
    lists:member(FunctionName, [behavior_info, behaviour_info]).

-spec module_naming_convention(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
module_naming_convention(Rule, ElvisConfig) ->
    Regex = elvis_rule:option(regex, Rule),
    ForbiddenRegex = elvis_rule:option(forbidden_regex, Rule),

    RegexAllow = re_compile(Regex),
    RegexBlock = re_compile(ForbiddenRegex),

    {nodes, ModuleNode} = elvis_code:find(#{
        of_types => [module],
        inside => elvis_code:root(Rule, ElvisConfig)
    }),

    ModuleName = module_name_from(ModuleNode, elvis_rule:file(Rule)),

    case re_run(ModuleName, RegexAllow) of
        nomatch ->
            [
                elvis_result:new_item(
                    "The name of this module is not acceptable by regular "
                    "expression '~s'",
                    [Regex]
                )
            ];
        {match, _} when RegexBlock =/= undefined ->
            case re_run(ModuleName, RegexBlock) of
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

module_name_from(ModuleNode, File) ->
    ModuleName =
        case ModuleNode of
            [Module] ->
                ktn_code:attr(value, Module);
            _ ->
                elvis_file:module(File)
        end,
    atom_to_list(ModuleName).

-spec state_record_and_type(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
state_record_and_type(Rule, ElvisConfig) ->
    Root = elvis_code:root(Rule, ElvisConfig),

    case is_otp_behaviour(Root) of
        true ->
            {nodes, StateRecordNodes} =
                elvis_code:find(#{
                    of_types => [record_attr],
                    inside => Root,
                    filtered_by => fun is_state_record/1
                }),
            HasStateRecord = StateRecordNodes =/= [],

            {nodes, StateTypeNodes} =
                elvis_code:find(#{
                    of_types => [type_attr, opaque],
                    inside => Root,
                    filtered_by => fun is_type_or_opaque_state/1
                }),
            HasStateType = StateTypeNodes =/= [],

            case {HasStateRecord, HasStateType} of
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

is_state_record(RecordAttrNode) ->
    ktn_code:attr(name, RecordAttrNode) =:= state.

is_type_or_opaque_state(TypeAttrOrOpaqueNode) ->
    case ktn_code:type(TypeAttrOrOpaqueNode) of
        type_attr ->
            is_type_state(TypeAttrOrOpaqueNode);
        opaque ->
            is_opaque_state(TypeAttrOrOpaqueNode)
    end.

is_type_state(TypeAttrOrOpaqueNode) ->
    ktn_code:attr(name, TypeAttrOrOpaqueNode) =:= state.

is_opaque_state(TypeAttrOrOpaqueNode) ->
    case ktn_code:attr(value, TypeAttrOrOpaqueNode) of
        {state, _, _} ->
            true;
        _ ->
            false
    end.

-spec no_spec_with_records(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_spec_with_records(Rule, ElvisConfig) ->
    {nodes, SpecWithRecordNodes} = elvis_code:find(#{
        of_types => [spec],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun spec_has_records/1
    }),

    [
        elvis_result:new_item(
            "an unexpected record was found in a spec; prefer creating a type for it and "
            "using that",
            [],
            #{node => SpecWithRecordNode}
        )
     || SpecWithRecordNode <- SpecWithRecordNodes
    ].

spec_has_records(SpecNode) ->
    {nodes, TypeNodes} = elvis_code:find(#{
        of_types => [type],
        inside => SpecNode,
        filtered_by => fun type_is_record/1,
        traverse => all
    }),
    TypeNodes =/= [].

type_is_record(TypeInSpecNode) ->
    ktn_code:attr(name, TypeInSpecNode) =:= record.

-spec dont_repeat_yourself(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
dont_repeat_yourself(Rule, ElvisConfig) ->
    MinComplexity = elvis_rule:option(min_complexity, Rule),

    Root = elvis_code:root(Rule, ElvisConfig),

    NodesWithRepeat = find_repeated_nodes(Root, MinComplexity),

    [
        elvis_result:new_item(
            "The code in the following (<line>, <column>) locations has the same structure: ~ts",
            [comma_separate_repetitions(NodeWithRepeat)]
        )
     || NodeWithRepeat <- NodesWithRepeat
    ].

comma_separate_repetitions(NodeWithRepeat) ->
    string:join(
        [io_lib:format("(~p, ~p)", [Line, Col]) || {Line, Col} <- NodeWithRepeat],
        ", "
    ).

-spec max_module_length(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
max_module_length(Rule, _ElvisConfig) ->
    MaxLength = elvis_rule:option(max_length, Rule),
    CountComments = elvis_rule:option(count_comments, Rule),
    CountWhitespace = elvis_rule:option(count_whitespace, Rule),
    CountDocs = elvis_rule:option(count_docs, Rule),

    {Src0, _} = elvis_file:src(elvis_rule:file(Rule)),
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

    DocLines = doc_lines(CountDocs, Docs),

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

doc_lines(true = _CountDocs, Docs) ->
    elvis_utils:split_all_lines(Docs, [trim]);
doc_lines(false, _Docs) ->
    [].

-spec max_anonymous_function_arity(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
max_anonymous_function_arity(Rule, ElvisConfig) ->
    MaxArity = elvis_rule:option(max_arity, Rule),

    {nodes, FunNodes} = elvis_code:find(#{
        of_types => ['fun', named_fun],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun has_clauses/1
    }),

    % We do this to recover the fun and arity
    FunArities = lists:filtermap(
        fun(FunNode) ->
            Arity = length(first_clause_args(FunNode)),
            case Arity > MaxArity of
                true ->
                    {true, {FunNode, Arity}};
                false ->
                    false
            end
        end,
        FunNodes
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

has_clauses(FunNode) ->
    %% Not having clauses means it's something like fun mod:f/10 and we don't want
    %% this rule to raise warnings for those. max_function_arity should take care of
    %% them.
    {nodes, ClauseNodes} = elvis_code:find(#{
        of_types => [clause],
        inside => FunNode
    }),
    ClauseNodes =/= [].

first_clause_args(FunNode) ->
    {nodes, [FirstClause | _]} = elvis_code:find(#{
        of_types => [clause],
        inside => FunNode
    }),
    ktn_code:node_attr(pattern, FirstClause).

-spec max_function_arity(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
max_function_arity(Rule, ElvisConfig) ->
    ExportedMaxArity = elvis_rule:option(max_arity, Rule),
    NonExportedMaxArity0 = elvis_rule:option(non_exported_max_arity, Rule),
    NonExportedMaxArity = specific_or_default(NonExportedMaxArity0, ExportedMaxArity),

    Root = elvis_code:root(Rule, ElvisConfig),

    {nodes, FunctionNodes0} = elvis_code:find(#{
        of_types => [function],
        inside => Root
    }),

    % We do this to recover the max arity (because it depends on "exported or not")
    FunctionNodeMaxArities = lists:filtermap(
        fun(FunctionNode) ->
            MaxArity = arity_for_function_exports(
                Root, FunctionNode, ExportedMaxArity, NonExportedMaxArity
            ),

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

arity_for_function_exports(Root, FunctionNode, ExportedMaxArity, NonExportedMaxArity) ->
    ExportedFunctions = exported_functions(Root),
    case is_exported_function(FunctionNode, ExportedFunctions) of
        true ->
            ExportedMaxArity;
        false ->
            NonExportedMaxArity
    end.

is_exported_function(FunctionNode, ExportedFunctions) ->
    Name = ktn_code:attr(name, FunctionNode),
    Arity = ktn_code:attr(arity, FunctionNode),
    lists:member({Name, Arity}, ExportedFunctions).

-spec max_anonymous_function_clause_length(elvis_rule:t(), elvis_config:t()) ->
    [elvis_result:item()].
max_anonymous_function_clause_length(Rule, ElvisConfig) ->
    MaxLength = elvis_rule:option(max_length, Rule),
    CountComments = elvis_rule:option(count_comments, Rule),
    CountWhitespace = elvis_rule:option(count_whitespace, Rule),

    {Src, _} = elvis_file:src(elvis_rule:file(Rule)),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    {zippers, ClauseZippers} = elvis_code:find(#{
        of_types => [clause],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(ClauseZipper) ->
                is_function_clause(ClauseZipper, ['fun', named_fun])
            end,
        filtered_from => zipper,
        traverse => all
    }),

    BigClauses = big_clauses(ClauseZippers, Lines, CountComments, CountWhitespace, MaxLength),

    lists:map(
        fun({ClauseZipper, ClauseNum, LineLen}) ->
            FunctionNode = zipper:node(zipper:up(ClauseZipper)),

            elvis_result:new_item(
                "the code for the ~s clause of the anonymous function has ~p lines, "
                "which is higher than the configured limit",
                [parse_clause_num(ClauseNum), LineLen],
                #{node => FunctionNode, limit => MaxLength}
            )
        end,
        lists:reverse(BigClauses)
    ).

-spec max_function_clause_length(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
max_function_clause_length(Rule, ElvisConfig) ->
    MaxLength = elvis_rule:option(max_length, Rule),
    CountComments = elvis_rule:option(count_comments, Rule),
    CountWhitespace = elvis_rule:option(count_whitespace, Rule),

    {Src, _} = elvis_file:src(elvis_rule:file(Rule)),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    {zippers, ClauseZippers} = elvis_code:find(#{
        of_types => [clause],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(ClauseZipper) ->
                is_function_clause(ClauseZipper, [function])
            end,
        filtered_from => zipper
    }),

    BigClauses = big_clauses(ClauseZippers, Lines, CountComments, CountWhitespace, MaxLength),

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

big_clauses(ClauseZippers, Lines, CountComments, CountWhitespace, MaxLength) ->
    % We do this to recover the clause number and apply the configured filters
    {BigClauses, _} = lists:foldl(
        fun(ClauseZipper, {BigClauses0, ClauseNum}) ->
            ClauseNode = zipper:node(ClauseZipper),
            FilteredLines = filtered_lines_in(ClauseNode, Lines, CountComments, CountWhitespace),
            LineLen = length(FilteredLines),
            AccOut =
                case LineLen > MaxLength of
                    true ->
                        [{ClauseZipper, ClauseNum, LineLen} | BigClauses0];
                    false ->
                        BigClauses0
                end,
            {AccOut, ClauseNum + 1}
        end,
        {[], 1},
        ClauseZippers
    ),
    BigClauses.

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

-spec max_anonymous_function_length(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
max_anonymous_function_length(Rule, ElvisConfig) ->
    MaxLength = elvis_rule:option(max_length, Rule),
    CountComments = elvis_rule:option(count_comments, Rule),
    CountWhitespace = elvis_rule:option(count_whitespace, Rule),

    {Src, _} = elvis_file:src(elvis_rule:file(Rule)),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    {nodes, FunctionNodes} = elvis_code:find(#{
        of_types => ['fun', named_fun],
        inside => elvis_code:root(Rule, ElvisConfig),
        traverse => all
    }),

    BigFunctions = big_functions(FunctionNodes, Lines, CountComments, CountWhitespace, MaxLength),

    lists:map(
        fun({FunctionNode, LineLen}) ->
            elvis_result:new_item(
                "the code for the anonymous function has ~p lines, which is higher than the "
                "configured limit",
                [LineLen],
                #{node => FunctionNode, limit => MaxLength}
            )
        end,
        BigFunctions
    ).

-spec max_function_length(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
max_function_length(Rule, ElvisConfig) ->
    MaxLength = elvis_rule:option(max_length, Rule),
    CountComments = elvis_rule:option(count_comments, Rule),
    CountWhitespace = elvis_rule:option(count_whitespace, Rule),

    {Src, _} = elvis_file:src(elvis_rule:file(Rule)),
    Lines = elvis_utils:split_all_lines(Src, [trim]),

    {nodes, FunctionNodes} = elvis_code:find(#{
        of_types => [function],
        inside => elvis_code:root(Rule, ElvisConfig)
    }),

    BigFunctions = big_functions(FunctionNodes, Lines, CountComments, CountWhitespace, MaxLength),

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

big_functions(FunctionNodes, Lines, CountComments, CountWhitespace, MaxLength) ->
    % We do this to apply the configured filters
    lists:filtermap(
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
    ).

-spec max_record_fields(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
max_record_fields(Rule, ElvisConfig) ->
    Root = elvis_code:root(Rule, ElvisConfig),

    MaxFields = elvis_rule:option(max_fields, Rule),

    {nodes, LargeRecordNodes} =
        elvis_code:find(#{
            of_types => [record_attr],
            inside => Root,
            filtered_by => fun(RecordNode) -> length(ktn_code:content(RecordNode)) > MaxFields end
        }),

    [
        elvis_result:new_item(
            "record ~p has ~p fields, which is higher than the configured limit",
            [ktn_code:attr(name, LargeRecordNode), length(ktn_code:content(LargeRecordNode))],
            #{node => LargeRecordNode, limit => MaxFields}
        )
     || LargeRecordNode <- LargeRecordNodes
    ].

-spec max_map_type_keys(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
max_map_type_keys(Rule, ElvisConfig) ->
    Root = elvis_code:root(Rule, ElvisConfig),
    MaxFields = elvis_rule:option(max_keys, Rule),
    max_map_type_keys_on_types(Root, MaxFields) ++ max_map_type_keys_on_opaques(Root, MaxFields).

max_map_type_keys_on_types(Root, MaxFields) ->
    {nodes, MapTypeNodes} =
        elvis_code:find(#{
            of_types => [type_attr],
            inside => Root,
            filtered_by => fun is_map_type_with_atom_keys/1
        }),

    [
        elvis_result:new_item(
            "map type ~p has ~p fields, which is higher than the configured limit",
            [
                ktn_code:attr(name, MapTypeNode),
                length(ktn_code:content(ktn_code:node_attr(type, MapTypeNode)))
            ],
            #{node => MapTypeNode, limit => MaxFields}
        )
     || MapTypeNode <- MapTypeNodes,
        length(ktn_code:content(ktn_code:node_attr(type, MapTypeNode))) > MaxFields
    ].

is_map_type_with_atom_keys(TypeAttrNode) ->
    TypeNode = ktn_code:node_attr(type, TypeAttrNode),
    ktn_code:attr(name, TypeNode) =:= map andalso
        all_type_keys_are_atoms(ktn_code:content(TypeNode)).

all_type_keys_are_atoms(TypeNodes) ->
    lists:all(
        fun(TypeNode) ->
            case ktn_code:content(TypeNode) of
                [] ->
                    %% -type t() :: map().
                    false;
                [KeyType | _] ->
                    ktn_code:type(KeyType) =:= atom
            end
        end,
        TypeNodes
    ).

max_map_type_keys_on_opaques(Root, MaxFields) ->
    {nodes, MapOpaqueNodes} =
        elvis_code:find(#{
            of_types => [opaque],
            inside => Root,
            filtered_by => fun is_map_opaque_with_atom_keys/1
        }),

    [
        elvis_result:new_item(
            "map type ~p has ~p fields, which is higher than the configured limit",
            [
                MapOpaqueName,
                length(erl_syntax:type_application_arguments(MapOpaqueTypeAST))
            ],
            #{node => MapOpaqueNode, limit => MaxFields}
        )
     || MapOpaqueNode <- MapOpaqueNodes,
        {MapOpaqueName, MapOpaqueTypeAST, _} <- [ktn_code:attr(value, MapOpaqueNode)],
        length(erl_syntax:type_application_arguments(MapOpaqueTypeAST)) > MaxFields
    ].

is_map_opaque_with_atom_keys(OpaqueNode) ->
    {_Name, TypeAST, _Params} = ktn_code:attr(value, OpaqueNode),
    erl_syntax:type(TypeAST) =:= map_type andalso
        all_opaque_keys_are_atoms(erl_syntax:type_application_arguments(TypeAST)).

all_opaque_keys_are_atoms(OpaqueASTs) ->
    %% for -opaque t() :: map()., OpaqueASTs =:= any.
    is_list(OpaqueASTs) andalso
        lists:all(
            fun(OpaqueAST) ->
                [KeyType | _] = erl_syntax:type_application_arguments(OpaqueAST),
                erl_syntax:type(KeyType) =:= atom
            end,
            OpaqueASTs
        ).

-spec no_call(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_call(Rule, ElvisConfig) ->
    DefaultFns = elvis_rule:option(no_call_functions, Rule),
    Msg = "an unexpected call to '~p:~p/~p' was found (check no_call list)",
    no_call_common(Rule, ElvisConfig, DefaultFns, Msg).

-spec no_debug_call(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_debug_call(Rule, ElvisConfig) ->
    DefaultFns = elvis_rule:option(debug_functions, Rule),
    Msg = "an unexpected debug call to '~p:~p/~p' was found",
    no_call_common(Rule, ElvisConfig, DefaultFns, Msg).

-spec no_common_caveats_call(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_common_caveats_call(Rule, ElvisConfig) ->
    DefaultFns = elvis_rule:option(caveat_functions, Rule),
    Msg = "the call to '~p:~p/~p' might have performance drawbacks or implicit behavior",
    no_call_common(Rule, ElvisConfig, DefaultFns, Msg).

-spec node_line_limits(ktn_code:tree_node()) -> {Min :: integer(), Max :: integer()}.
node_line_limits(FunctionNode) ->
    Zipper = elvis_code:zipper(FunctionNode),
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

-spec no_nested_try_catch(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_nested_try_catch(Rule, ElvisConfig) ->
    {nodes, TryExprNodes} = elvis_code:find(#{
        of_types => ['try'],
        inside => elvis_code:root(Rule, ElvisConfig)
    }),

    InnerTryExprNodes = inner_try_exprs(TryExprNodes),

    [
        elvis_result:new_item(
            "an unexpected nested 'try...catch' expression was found",
            [],
            #{node => InnerTryExprNode}
        )
     || InnerTryExprNode <- InnerTryExprNodes
    ].

inner_try_exprs(TryExprNodes) ->
    [
        TryExprContentNode
     || TryExprNode <- TryExprNodes,
        TryExprContentNode <- ktn_code:content(TryExprNode),
        ktn_code:type(TryExprContentNode) =:= 'try'
    ].

-spec no_successive_maps(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_successive_maps(Rule, ElvisConfig) ->
    {nodes, MapExprNodes} = elvis_code:find(#{
        of_types => [map],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_successive_map/1,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "an unexpected map update after map construction/update was found",
            [],
            #{node => MapExprNode}
        )
     || MapExprNode <- MapExprNodes
    ].

is_successive_map(MapExprNode) ->
    InnerVar = ktn_code:node_attr(var, MapExprNode),
    ktn_code:type(InnerVar) =:= map.

-spec atom_naming_convention(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
atom_naming_convention(Rule, ElvisConfig) ->
    {zippers, AtomZippers} = elvis_code:find(#{
        of_types => [atom],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun parent_is_not_remote/1,
        filtered_from => zipper,
        traverse => all
    }),

    lists:filtermap(
        fun(AtomZipper) ->
            case atom_name_matches_regexes(AtomZipper, Rule) of
                ok ->
                    false;
                {not_acceptable, Kind, AtomName, RegexAllow} ->
                    {true,
                        elvis_result:new_item(
                            "the name of ~ts ~p is not acceptable by regular "
                            "expression '~s'",
                            [Kind, AtomName, RegexAllow],
                            #{zipper => AtomZipper}
                        )};
                {blocked, Kind, AtomName, RegexBlock} ->
                    {true,
                        elvis_result:new_item(
                            "the name of ~ts ~p is forbidden by regular "
                            "expression '~s'",
                            [Kind, AtomName, RegexBlock],
                            #{zipper => AtomZipper}
                        )}
            end
        end,
        AtomZippers
    ).

atom_name_matches_regexes(AtomZipper, Rule) ->
    Regex = elvis_rule:option(regex, Rule),
    ForbiddenRegex = elvis_rule:option(forbidden_regex, Rule),
    RegexEnclosed0 = elvis_rule:option(enclosed_atoms, Rule),
    RegexEnclosed = specific_or_default(RegexEnclosed0, Regex),
    ForbiddenEnclosedRegex0 = elvis_rule:option(forbidden_enclosed_regex, Rule),
    ForbiddenEnclosedRegex = specific_or_default(ForbiddenEnclosedRegex0, ForbiddenRegex),

    {AtomName0, AtomNodeValue} = atom_name_and_node_value(AtomZipper),
    {IsEnclosed, AtomName} = string_strip_enclosed(AtomName0),
    IsExceptionClass = is_exception_or_non_reversible(AtomNodeValue),

    RegexAllow = re_compile_for_atom_type(IsEnclosed, Regex, RegexEnclosed),
    RegexBlock = re_compile_for_atom_type(IsEnclosed, ForbiddenRegex, ForbiddenEnclosedRegex),

    AtomNameUnicode = unicode:characters_to_list(AtomName),
    case re_run(AtomNameUnicode, RegexAllow) of
        _ when IsExceptionClass, not IsEnclosed ->
            ok;
        nomatch when not IsEnclosed ->
            {not_acceptable, "atom", AtomName, RegexAllow};
        nomatch when IsEnclosed ->
            {not_acceptable, "enclosed atom", AtomName, RegexAllow};
        {match, _Captured} when RegexBlock =/= undefined ->
            % We check for forbidden names only after accepted names
            case re_run(AtomNameUnicode, RegexBlock) of
                _ when IsExceptionClass, not IsEnclosed ->
                    ok;
                {match, _} when not IsEnclosed ->
                    {blocked, "atom", AtomName, RegexBlock};
                {match, _} when IsEnclosed ->
                    {blocked, "enclosed atom", AtomName, RegexBlock};
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

atom_name_and_node_value(AtomZipper) ->
    AtomNode = zipper:node(AtomZipper),
    {ktn_code:attr(text, AtomNode), ktn_code:attr(value, AtomNode)}.

-spec no_init_lists(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_init_lists(Rule, ElvisConfig) ->
    ConfigBehaviors = elvis_rule:option(behaviours, Rule),

    Root = elvis_code:root(Rule, ElvisConfig),

    InitClauseNodes =
        case is_behaviour_in(Root, ConfigBehaviors) of
            true ->
                {nodes, FunctionNodes} = elvis_code:find(#{
                    of_types => [function],
                    inside => Root,
                    filtered_by => fun is_init_1/1
                }),

                case FunctionNodes of
                    [] ->
                        [];
                    [Init1Fun] ->
                        Content = ktn_code:content(Init1Fun),
                        ListAttrClauses = list_nodes(Content),

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
            "an avoidable list was found as argument to 'init' callback; prefer tuples, maps "
            "or records",
            [],
            #{node => InitClauseNode}
        )
     || InitClauseNode <- InitClauseNodes
    ].

is_init_1(FunctionNode) ->
    ktn_code:attr(name, FunctionNode) =:= init andalso
        ktn_code:attr(arity, FunctionNode) =:= 1.

list_nodes(Content) ->
    lists:filter(
        fun(Clause) ->
            [Attribute] = ktn_code:node_attr(pattern, Clause),
            is_list_node(Attribute)
        end,
        Content
    ).

is_behaviour_in(Root, ConfigBehaviors) ->
    {nodes, Behaviours} = elvis_code:find(#{of_types => [behaviour, behavior], inside => Root}),
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
    lists:any(fun is_list_node/1, Content);
is_list_node(_) ->
    false.

-spec ms_transform_included(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
ms_transform_included(Rule, ElvisConfig) ->
    Root = elvis_code:root(Rule, ElvisConfig),

    case has_include_ms_transform(Root) of
        true ->
            [];
        false ->
            {nodes, FunctionNodes} = elvis_code:find(#{
                of_types => [call],
                inside => Root,
                filtered_by => fun is_ets_fun2ms/1
            }),

            [
                elvis_result:new_item(
                    "'ets:fun2ms/1' is used but the module is missing "
                    "'-include_lib(\"stdlib/include/ms_transform.hrl\").'",
                    [],
                    #{node => FunctionNode}
                )
             || FunctionNode <- FunctionNodes
            ]
    end.

is_ets_fun2ms(Node) ->
    FunctionInNode = ktn_code:node_attr(function, Node),

    FunctionRef0 = ktn_code:node_attr(function, FunctionInNode),
    FunctionRef = ktn_code:attr(value, FunctionRef0),

    ModuleRef0 = ktn_code:node_attr(module, FunctionInNode),
    ModuleRef = ktn_code:attr(value, ModuleRef0),

    {ModuleRef, FunctionRef} =:= {ets, fun2ms}.

-spec no_boolean_in_comparison(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_boolean_in_comparison(Rule, ElvisConfig) ->
    {nodes, OpNodes} = elvis_code:find(#{
        of_types => [op],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_boolean_in_comparison/1,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "an avoidable comparison to boolean was found",
            [],
            #{node => OpNode}
        )
     || OpNode <- lists:uniq(OpNodes)
    ].

is_boolean_in_comparison(OpNode) ->
    is_op(OpNode, ['==', '=:=', '/=', '=/=']) andalso operates_on_boolean(OpNode).

is_op(OpNode, Ops) ->
    Operation = ktn_code:attr(operation, OpNode),
    lists:member(Operation, Ops).

operates_on_boolean(OpNode) ->
    lists:any(
        fun(OpContentNode) ->
            is_boolean(ktn_code:attr(value, OpContentNode))
        end,
        ktn_code:content(OpNode)
    ).

-spec no_receive_without_timeout(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_receive_without_timeout(Rule, ElvisConfig) ->
    {nodes, ReceiveExprNodes} = elvis_code:find(#{
        of_types => ['receive'],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_receive_without_timeout/1
    }),

    [
        elvis_result:new_item(
            "a 'receive' expression was found without an 'after' clause; "
            "prefer to include 'after' in 'receive' expressions",
            [],
            #{node => ReceiveExprNode}
        )
     || ReceiveExprNode <- ReceiveExprNodes
    ].

is_receive_without_timeout(Receive) ->
    {nodes, ReceiveAfterNodes} = elvis_code:find(#{
        of_types => [receive_after],
        inside => Receive
    }),
    ReceiveAfterNodes =:= [].

-spec strict_term_equivalence(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
strict_term_equivalence(Rule, ElvisConfig) ->
    Operators = #{'==' => '=:=', '/=' => '=/='},

    {nodes, OpNodes} = elvis_code:find(#{
        of_types => [op],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(OpNode) ->
                is_op(OpNode, maps:keys(Operators))
            end,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "unexpected weak comparison operator ~p was found; prefer ~p",
            [
                ktn_code:attr(operation, OpNode),
                maps:get(ktn_code:attr(operation, OpNode), Operators)
            ],
            #{node => OpNode}
        )
     || OpNode <- OpNodes
    ].

-spec no_operation_on_same_value(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_operation_on_same_value(Rule, ElvisConfig) ->
    InterestingOps = elvis_rule:option(operations, Rule),

    {nodes, OpNodes} = elvis_code:find(#{
        of_types => [op],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(OpNode) ->
                is_op(OpNode, InterestingOps) andalso same_value_on_both_sides(OpNode)
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
            nodes_same_except_location(Left, Right);
        _ ->
            false
    end.

nodes_same_except_location([_ | _], []) ->
    false;
nodes_same_except_location([], [_ | _]) ->
    false;
nodes_same_except_location([], []) ->
    true;
nodes_same_except_location([LeftNode | LeftNodes], [RightNode | RigthNodes]) ->
    nodes_same_except_location(LeftNode, RightNode) andalso
        nodes_same_except_location(LeftNodes, RigthNodes);
nodes_same_except_location(LeftNode, RightNode) ->
    %% If we're evaluating a function, then even if we evaluate the same function on both sides,
    %% the results may be different.
    nodes_not_call(LeftNode, RightNode) andalso
        nodes_same_type(LeftNode, RightNode) andalso
        nodes_same_attrs(LeftNode, RightNode) andalso
        nodes_same_attrs_except_location(LeftNode, RightNode) andalso
        nodes_same_except_location(ktn_code:content(LeftNode), ktn_code:content(RightNode)).

nodes_not_call(LeftNode, RightNode) ->
    not is_call(LeftNode) andalso not is_call(RightNode).

nodes_same_type(LeftNode, RightNode) ->
    ktn_code:type(LeftNode) =:= ktn_code:type(RightNode).

nodes_same_attrs(LeftNode, RightNode) ->
    maps:remove(location, maps:get(attrs, LeftNode)) =:=
        maps:remove(location, maps:get(attrs, RightNode)).

nodes_same_attrs_except_location(#{node_attrs := LeftAttrs}, #{node_attrs := RightAttrs}) ->
    nodes_same_attr_keys(LeftAttrs, RightAttrs) andalso
        lists:all(
            fun(AttrKey) ->
                nodes_same_except_location(
                    maps:get(AttrKey, LeftAttrs), maps:get(AttrKey, RightAttrs)
                )
            end,
            maps:keys(LeftAttrs)
        );
nodes_same_attrs_except_location(LeftNode, RightNode) ->
    not maps:is_key(node_attrs, LeftNode) andalso not maps:is_key(node_attrs, RightNode).

nodes_same_attr_keys(LeftAttrs, RightAttrs) ->
    maps:keys(LeftAttrs) =:= maps:keys(RightAttrs).

has_include_ms_transform(Root) ->
    {nodes, IncludeLibNodes} = elvis_code:find(#{
        of_types => [include_lib],
        inside => Root,
        filtered_by => fun is_ms_transform_hrl/1
    }),
    IncludeLibNodes =/= [].

is_ms_transform_hrl(IncludeLibNode) ->
    ktn_code:attr(value, IncludeLibNode) =:= "stdlib/include/ms_transform.hrl".

-spec no_throw(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_throw(Rule, ElvisConfig) ->
    {nodes, ThrowNodes} = elvis_code:find(#{
        of_types => [call],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_throw/1
    }),

    [
        elvis_result:new_item(
            "an avoidable call to 'throw/1' was found; prefer 'exit/1' or 'error/1'",
            [],
            #{node => ThrowNode}
        )
     || ThrowNode <- ThrowNodes
    ].

is_throw(OpNode) ->
    lists:any(
        fun(MFA) ->
            is_call(OpNode, MFA)
        end,
        [{throw, 1}, {erlang, throw, 1}]
    ).

-spec no_dollar_space(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_dollar_space(Rule, ElvisConfig) ->
    {nodes, CharNodes} = elvis_code:find(#{
        of_types => [char],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_dollar_space/1,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "unexpected character '$ ' was found; prefer $\\s",
            [],
            #{node => CharNode}
        )
     || CharNode <- CharNodes
    ].

is_dollar_space(CharNode) ->
    ktn_code:attr(text, CharNode) =:= "$ ".

-spec no_author(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_author(Rule, ElvisConfig) ->
    {nodes, AuthorNodes} = elvis_code:find(#{
        of_types => [author],
        inside => elvis_code:root(Rule, ElvisConfig)
    }),

    [
        elvis_result:new_item(
            "avoidable attribute '-author' was found",
            [],
            #{node => AuthorNode}
        )
     || AuthorNode <- AuthorNodes
    ].

-spec no_import(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_import(Rule, ElvisConfig) ->
    {nodes, ImportNodes} = elvis_code:find(#{
        of_types => [import],
        inside => elvis_code:root(Rule, ElvisConfig)
    }),

    [
        elvis_result:new_item(
            "unexpected attribute '-import' was found",
            [],
            #{node => ImportNode}
        )
     || ImportNode <- ImportNodes
    ].

-spec no_catch_expressions(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_catch_expressions(Rule, ElvisConfig) ->
    {zippers, CatchExprZippers} = elvis_code:find(#{
        of_types => ['catch'],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_from => zipper,
        filtered_by => fun(Zipper) -> not result_discarded(Zipper) end
    }),

    [
        elvis_result:new_item(
            "an unexpected 'catch' expression was found; prefer a 'try' expression",
            [],
            #{node => zipper:node(CatchExprZipper)}
        )
     || CatchExprZipper <- lists:uniq(CatchExprZippers)
    ].

%% @doc is this node in a _ = ... expression, where the result is explicitly discarded?
result_discarded(Zipper) ->
    Parent = zipper:node(zipper:up(Zipper)),
    case ktn_code:type(Parent) of
        match ->
            [LeftSide | _] = ktn_code:content(Parent),
            ktn_code:type(LeftSide) =:= var andalso ktn_code:attr(name, LeftSide) =:= '_';
        _ ->
            false
    end.

-spec no_single_clause_case(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_single_clause_case(Rule, ElvisConfig) ->
    {nodes, CaseExprs} = elvis_code:find(#{
        of_types => ['case'],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_single_clause_case/1
    }),

    [
        elvis_result:new_item(
            "an avoidable single-clause 'case' expression was found",
            [],
            #{node => CaseExpr}
        )
     || CaseExpr <- CaseExprs
    ].

is_single_clause_case(CaseExpr) ->
    length(case_clauses_in(CaseExpr)) =:= 1.

case_clauses_in(Node) ->
    [
        Clause
     || SubNode <- ktn_code:content(Node),
        ktn_code:type(SubNode) =:= case_clauses,
        Clause <- ktn_code:content(SubNode)
    ].

-spec no_single_match_maybe(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_single_match_maybe(Rule, ElvisConfig) ->
    {nodes, MaybeNodes} = elvis_code:find(#{
        of_types => ['maybe'],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_single_match_maybe/1
    }),

    [
        elvis_result:new_item(
            "an avoidable single-match 'maybe' block was found",
            [],
            #{node => MaybeNode}
        )
     || MaybeNode <- MaybeNodes
    ].

is_single_match_maybe(MaybeNode) ->
    length(ktn_code:content(MaybeNode)) =:= 1.

-spec no_match_in_condition(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
no_match_in_condition(Rule, ElvisConfig) ->
    {nodes, CaseExprNodes} = elvis_code:find(#{
        of_types => [case_expr],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(CaseExprNode) ->
                has_match_child(CaseExprNode) orelse has_match_child_block(CaseExprNode)
            end
    }),

    [
        elvis_result:new_item(
            "an avoidable match condition in a 'case' expression was found; prefer matching "
            "in 'case' clauses",
            [],
            #{node => CaseExprNode}
        )
     || CaseExprNode <- CaseExprNodes
    ].

is_match(Node) ->
    lists:member(ktn_code:type(Node), match_operators()).

has_match_child(Node) ->
    %% case_expr followed by a match
    lists:any(fun is_match/1, ktn_code:content(Node)).

has_match_child_block(CaseExprNode) ->
    %% case_expr followed by a block which contains a match in the first layer
    lists:any(
        fun(CaseExprContent) ->
            ktn_code:type(CaseExprContent) =:= block andalso
                has_match_child(CaseExprContent)
        end,
        ktn_code:content(CaseExprNode)
    ).

-spec numeric_format(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
numeric_format(Rule, ElvisConfig) ->
    Regex = elvis_rule:option(regex, Rule),
    IntRegex0 = elvis_rule:option(int_regex, Rule),
    IntRegex = specific_or_default(IntRegex0, Regex),
    FloatRegex0 = elvis_rule:option(float_regex, Rule),
    FloatRegex = specific_or_default(FloatRegex0, Regex),

    Root = elvis_code:root(Rule, ElvisConfig),

    {nodes, IntegerNodes} = elvis_code:find(#{
        of_types => [integer],
        inside => Root,
        filtered_by =>
            fun(NumberNode) ->
                is_not_acceptable_number(NumberNode, IntRegex)
            end
    }),

    {nodes, FloatNodes} = elvis_code:find(#{
        of_types => [float],
        inside => Root,
        filtered_by =>
            fun(NumberNode) ->
                is_not_acceptable_number(NumberNode, FloatRegex)
            end
    }),

    [
        elvis_result:new_item(
            "the format of number '~s' is not acceptable by regular "
            "expression '~s'",
            [ktn_code:attr(text, NumberNode), Regex],
            #{node => NumberNode}
        )
     || NumberNode <- IntegerNodes ++ FloatNodes
    ].

is_not_acceptable_number(NumberNode, Regex) ->
    Number = ktn_code:attr(text, NumberNode),
    Number =/= undefined andalso re_run(Number, Regex) =:= nomatch.

-spec prefer_unquoted_atoms(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
prefer_unquoted_atoms(Rule, ElvisConfig) ->
    {nodes, AtomNodes} = elvis_code:find(#{
        of_types => [atom],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun doesnt_need_quotes/1,
        traverse => all
    }),

    lists:map(
        fun(AtomNode) ->
            elvis_result:new_item(
                "unnecessarily quoted atom ~s was found; prefer removing the quotes when "
                "not syntactically required",
                [ktn_code:attr(text, AtomNode)],
                #{node => AtomNode}
            )
        end,
        AtomNodes
    ).

doesnt_need_quotes(AtomNode) ->
    AtomName0 = ktn_code:attr(text, AtomNode),
    case re:run(AtomName0, "^'[a-z][a-zA-Z0-9_@]*'$", [{capture, none}]) of
        match ->
            AtomName = string:trim(AtomName0, both, "'"),
            Atom = list_to_atom(AtomName),
            Atom =/= 'maybe' andalso not erl_scan:f_reserved_word(Atom);
        _ ->
            false
    end.

-spec behaviour_spelling(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
behaviour_spelling(Rule, ElvisConfig) ->
    Spelling = elvis_rule:option(spelling, Rule),

    {nodes, BehaviourNodes} = elvis_code:find(#{
        of_types => [behaviour, behavior],
        inside => elvis_code:root(Rule, ElvisConfig),
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

-spec guard_operators(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
guard_operators(Rule, ElvisConfig) ->
    case elvis_rule:option(preferred_syntax, Rule) of
        per_expression ->
            {zippers, GuardedClauseZippers} = elvis_code:find(#{
                of_types => [clause],
                inside => elvis_code:root(Rule, ElvisConfig),
                filtered_by =>
                    fun(ClauseZipper) ->
                        has_guards(zipper:node(ClauseZipper))
                    end,
                filtered_from => zipper,
                traverse => all
            }),
            GuardedExpressionNodes =
                [
                    zipper:node(zipper:up(GuardedClauseZipper))
                 || GuardedClauseZipper <- GuardedClauseZippers
                ],
            check_guard_operators(per_expression, GuardedExpressionNodes);
        PreferredSyntax ->
            {nodes, GuardedClauseNodes} = elvis_code:find(#{
                of_types => [clause],
                inside => elvis_code:root(Rule, ElvisConfig),
                filtered_by => fun has_guards/1
            }),
            check_guard_operators(PreferredSyntax, GuardedClauseNodes)
    end.

check_guard_operators(punctuation, ClauseNodes) ->
    [
        elvis_result:new_item(
            "an unexpected shortcircuit operator was found; prefer ';' or ','",
            [],
            #{node => ClauseNode}
        )
     || ClauseNode <- ClauseNodes,
        has_guard_defined_with_words(ClauseNode)
    ];
check_guard_operators(words, ClauseNodes) ->
    [
        elvis_result:new_item(
            "one or more unexpected punctutation operators were found;"
            " prefer 'andalso' or 'orelse'",
            [],
            #{node => ClauseNode}
        )
     || ClauseNode <- ClauseNodes,
        has_guard_defined_with_punctuation(ClauseNode)
    ];
check_guard_operators(per_clause, ClauseNodes) ->
    [
        elvis_result:new_item(
            "an unexpected combination of punctuation and shortcircuit operators was found",
            [],
            #{node => ClauseNode}
        )
     || ClauseNode <- ClauseNodes,
        has_guard_defined_with_punctuation(ClauseNode),
        has_guard_defined_with_words(ClauseNode)
    ];
check_guard_operators(per_expression, ExpressionNodes) ->
    lists:uniq([
        elvis_result:new_item(
            "an unexpected combination of punctuation and shortcircuit operators was found",
            [],
            #{node => ExpressionNode}
        )
     || ExpressionNode <- ExpressionNodes,
        {nodes, []} =/=
            elvis_code:find(#{
                of_types => [clause],
                inside => ExpressionNode,
                filtered_by => fun has_guard_defined_with_punctuation/1
            }) andalso
            {nodes, []} =/=
                elvis_code:find(#{
                    of_types => [clause],
                    inside => ExpressionNode,
                    filtered_by => fun has_guard_defined_with_words/1
                })
    ]).

has_guards(ClauseNode) ->
    [] =/= ktn_code:node_attr(guards, ClauseNode).

%% @doc If guards are joined by ; or guard-expressions are joined by , ktn_code reports them
%%      as lists of lists.
%%      If they only use words, then we just have [[#{type := op, attrs := #{operation = '...'}}]]
has_guard_defined_with_punctuation(ClauseNode) ->
    length(ktn_code:node_attr(guards, ClauseNode)) > 1 orelse
        length(ktn_code:node_attr(guards, ClauseNode)) =:= 1 andalso
            length(hd(ktn_code:node_attr(guards, ClauseNode))) > 1.

has_guard_defined_with_words(ClauseNode) ->
    [] =/=
        [
            GuardExpression
         || Guard <- ktn_code:node_attr(guards, ClauseNode),
            GuardExpression <- Guard,
            is_two_sided_boolean_op(GuardExpression)
        ].

%% @doc This function returns true for X andalso Y, X orelse Y, X and Y, etc...
%%      It also returns true for not not not not X andalso Y
%%      But it returns false for not not not X.
is_two_sided_boolean_op(Node) ->
    op =:= ktn_code:type(Node) andalso
        lists:member(ktn_code:attr(operation, Node), ['and', 'or', 'andalso', 'orelse']) orelse
        ktn_code:attr(operation, Node) =:= 'not' andalso
            is_two_sided_boolean_op(hd(ktn_code:content(Node))).

-spec param_pattern_matching(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
param_pattern_matching(Rule, ElvisConfig) ->
    Side = elvis_rule:option(side, Rule),

    {zippers, ClauseZippers} = elvis_code:find(#{
        of_types => [clause],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(ClauseZipper) ->
                is_function_clause(ClauseZipper, [function, 'fun', named_fun])
            end,
        filtered_from => zipper,
        traverse => all
    }),

    MatchesInFunctionClauses = matches_in_function_clauses(ClauseZippers),
    MatchVars = match_vars_at(Side, MatchesInFunctionClauses),

    [
        elvis_result:new_item(
            "variable '~s' is used to match an argument, but placed on "
            "the wrong side of it; prefer the ~p side",
            [atom_to_list(ktn_code:attr(name, Var)), Side],
            #{node => Match}
        )
     || {Match, Var} <- MatchVars
    ].

match_vars_at(Side, MatchesInFunctionClauses) ->
    lists:filtermap(
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
    ).

matches_in_function_clauses(ClauseZippers) ->
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
    ).

is_function_clause(ClauseZipper, ParentNodeTypes) ->
    ClauseParent = zipper:up(ClauseZipper),
    ParentNode = zipper:node(ClauseParent),
    ParentNodeType = ktn_code:type(ParentNode),
    lists:member(ParentNodeType, ParentNodeTypes).

-spec generic_type(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
generic_type(Rule, ElvisConfig) ->
    PreferredType = elvis_rule:option(preferred_type, Rule),

    {nodes, TypeNodes} = elvis_code:find(#{
        of_types => [type, callback],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(TypeNode) ->
                is_inconsistent_generic_type(TypeNode, PreferredType)
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

is_inconsistent_generic_type(TypeNode, PreferredType) ->
    TypeNodeName = ktn_code:attr(name, TypeNode),
    IsTermOrAny = lists:member(TypeNodeName, [term, any]),
    IsTermOrAny andalso TypeNodeName =/= PreferredType.

-spec always_shortcircuit(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
always_shortcircuit(Rule, ElvisConfig) ->
    Operators = #{'and' => 'andalso', 'or' => 'orelse'},

    {nodes, OpNodes} = elvis_code:find(#{
        of_types => [op],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(OpNode) ->
                is_op(OpNode, maps:keys(Operators))
            end,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "unexpected non-shortcircuiting operator ~p was found; prefer ~p",
            [
                ktn_code:attr(operation, OpNode),
                maps:get(ktn_code:attr(operation, OpNode), Operators)
            ],
            #{node => OpNode}
        )
     || OpNode <- OpNodes
    ].

-spec simplify_anonymous_functions(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
simplify_anonymous_functions(Rule, ElvisConfig) ->
    {nodes, FunNodes} = elvis_code:find(#{
        of_types => ['fun'],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => fun is_simple_anonymous_function/1,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "an unnecessary anonymous function wrapper was found; prefer 'fun M:F/A'",
            [],
            #{node => Fun}
        )
     || Fun <- FunNodes
    ].

%% @doc Has this anonymous function just one clause that is a call to a
%%      regular function with the same args? i.e., something like
%%      fun() -> x() end ; or
%%      fun(A) -> some:funct(A) end ; or
%%      fun(A, B, C) -> x:y(A, B, C) end
%%      Note that we assume that FunNode is, in fact, an anonymous function node.
is_simple_anonymous_function(FunNode) ->
    case
        elvis_code:find(#{
            of_types => [clause],
            inside => FunNode
        })
    of
        {nodes, [Clause]} ->
            case ktn_code:content(Clause) of
                [Expression] ->
                    ktn_code:node_attr(guards, Clause) =:= [] andalso
                        ktn_code:type(Expression) =:= call andalso
                        nodes_same_except_location(
                            ktn_code:node_attr(pattern, Clause), ktn_code:content(Expression)
                        );
                _ ->
                    false
            end;
        _ ->
            false
    end.

-spec prefer_strict_generators(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
prefer_strict_generators(Rule, ElvisConfig) ->
    WeakGenerators = #{b_generate => '<=', generate => '<-', m_generate => '<-'},
    StrictGenerators = #{b_generate => '<:=', generate => '<:-', m_generate => '<:-'},

    {nodes, GenNodes} = elvis_code:find(#{
        of_types => maps:keys(WeakGenerators),
        inside => elvis_code:root(Rule, ElvisConfig),
        traverse => all
    }),

    [
        elvis_result:new_item(
            "unexpected weak generator ~p was found; prefer ~p",
            [
                maps:get(ktn_code:type(GenNode), WeakGenerators),
                maps:get(ktn_code:type(GenNode), StrictGenerators)
            ],
            #{node => GenNode}
        )
     || GenNode <- GenNodes
    ].

-spec prefer_include(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
prefer_include(Rule, ElvisConfig) ->
    {nodes, FunNodes} = elvis_code:find(#{
        of_types => [include_lib],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(#{attrs := #{value := Value}}) ->
                filename:basename(Value) =:= Value
            end,
        traverse => all
    }),

    [
        elvis_result:new_item(
            "an unexpected '-include_lib' was found; prefer '-include'",
            [],
            #{node => Fun}
        )
     || Fun <- FunNodes
    ].

-spec export_used_types(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
export_used_types(Rule, ElvisConfig) ->
    Root = elvis_code:root(Rule, ElvisConfig),

    case is_otp_behaviour(Root) of
        false ->
            ExportedFunctions = exported_functions(Root),
            ExportedTypes = exported_types(Root),

            SpecNodes = spec_nodes(Root, ExportedFunctions),
            UsedTypes = used_types(SpecNodes),

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

spec_nodes(Root, ExportedFunctions) ->
    {nodes, SpecNodes} = elvis_code:find(#{
        of_types => [spec],
        inside => Root,
        filtered_by =>
            fun(SpecNode) ->
                is_exported_function(SpecNode, ExportedFunctions)
            end
    }),
    SpecNodes.

used_types(SpecNodes) ->
    lists:usort(
        lists:flatmap(
            fun(SpecNode) ->
                {nodes, UserTypeNodes} = elvis_code:find(#{
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
    ).

-spec private_data_types(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
private_data_types(Rule, ElvisConfig) ->
    TypesToCheck = elvis_rule:option(apply_to, Rule),

    Root = elvis_code:root(Rule, ElvisConfig),
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
    {nodes, TypeAttrNodes} = elvis_code:find(#{
        of_types => [type_attr],
        inside => Root,
        filtered_by =>
            fun(TypeAttrNode) ->
                is_public_data_type_in(TypesToCheck, TypeAttrNode)
            end,
        traverse => all
    }),

    NameArities = lists:map(
        fun(#{attrs := #{name := Name}, node_attrs := #{args := Args}}) ->
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

is_public_data_type_in(TypesToCheck, TypeAttrNode) ->
    TypeAttrNodeType = ktn_code:node_attr(type, TypeAttrNode),
    TypeAttrNodeType =/= undefined andalso
        lists:member(ktn_code:attr(name, TypeAttrNodeType), TypesToCheck).

-spec macro_definition_parentheses(elvis_rule:t(), elvis_config:t()) -> [elvis_result:item()].
macro_definition_parentheses(Rule, ElvisConfig) ->
    TypeMismatchingDefine =
        fun(#{attrs := #{value := [Elem1, Elem2]}}) ->
            case {macro_attr_type(Elem1), macro_attr_type(Elem2)} of
                {call, call} ->
                    false;
                {call, _} ->
                    true;
                {var, tree} ->
                    is_stringified_function(
                        get_tree_content(Elem2)
                    );
                _ ->
                    false
            end
        end,

    {nodes, InvalidMacroNodes} = elvis_code:find(#{
        of_types => [define],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by => TypeMismatchingDefine
    }),

    lists:map(
        fun(Node) ->
            elvis_result:new_item(
                "Invalid parenthesis at a macro definition."
                "Functions should contain parenthesis, constants should not",
                [],
                #{node => Node}
            )
        end,
        InvalidMacroNodes
    ).

macro_attr_type({Type, _, _}) ->
    Type;
macro_attr_type({Type, _, _, _}) ->
    Type.

is_stringified_function(Tree) ->
    re:run(Tree, "^[a-zA-Z_][a-zA-Z0-9_]* ?:? ?[a-zA-Z0-9_]*\\([^)]*\\)$", [{capture, none}]) =:=
        match.

get_tree_content({tree, text, _, Content}) ->
    Content.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec map_type_declarations_to_location(ktn_code:tree_node()) ->
    #{{atom(), number()} => number()}.
map_type_declarations_to_location(Root) ->
    {nodes, AllTypes} = elvis_code:find(#{of_types => [type_attr], inside => Root}),
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

line_is_comment(Line) ->
    case re_run(Line, "^[ \t]*%") of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

line_is_whitespace(Line) ->
    case re_run(Line, "^[ \t]*$") of
        nomatch ->
            false;
        {match, _} ->
            true
    end.

original_macro_name_from(MacroNode) ->
    MacroNodeValue = ktn_code:attr(value, MacroNode),
    MacroAsAtom = macro_as_atom(false, [call, var, atom], MacroNodeValue),
    atom_to_list(MacroAsAtom).

stripped_macro_name_from(MacroNode) ->
    MacroNameOriginal = original_macro_name_from(MacroNode),
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
    How :: {should_have, []} | {should_not_have, [{string(), {ok, MP}}]}
) ->
    char()
when
    % MP is re:mp/0
    MP :: _.
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
        {_, right, LenLine} when How =:= should_have andalso ColToCheck > LenLine ->
            SpaceChar;
        {_, right, LenLine} when How =:= should_not_have andalso ColToCheck > LenLine ->
            "";
        _ when How =:= should_have orelse TextRegex =:= nomatch andalso ColToCheck > 1 ->
            lists:nth(ColToCheck, TextLineStr);
        _ when
            How =:= should_not_have andalso
                ColToCheck > 1 andalso
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
    IncrementOne = [function, 'case', 'if', try_case, try_catch, named_fun, receive_case, 'maybe'],
    case lists:member(Type, IncrementOne) of
        true ->
            1;
        false ->
            0
    end.

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

parent_is_not_remote(Zipper) ->
    case zipper:up(Zipper) of
        undefined ->
            true;
        ParentZipper ->
            Parent = zipper:node(ParentZipper),
            ktn_code:type(Parent) =/= remote
    end.

is_otp_behaviour(Root) ->
    OtpSet = sets:from_list([gen_server, gen_event, gen_fsm, gen_statem, supervisor_bridge]),
    {nodes, Behaviors} = elvis_code:find(#{of_types => [behaviour, behavior], inside => Root}),
    case Behaviors of
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

find_repeated_nodes(Root, MinComplexity) ->
    TypeAttrs = #{var => [location, name, text], clause => [location, text]},

    FoldFun =
        fun(Node, Map) ->
            Zipper = elvis_code:zipper(Node),
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
    ZipperRoot = elvis_code:zipper(Root),
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
            {Key, remove_attrs_zipper(elvis_code:zipper(Value), TypeAttrs)}
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
    Zipper = elvis_code:zipper(Parent),
    zipper:filter(fun(Child) -> Child =:= Node end, Zipper) =/= [].

no_call_common(Rule, ElvisConfig, NoCallFuns, Msg) ->
    {nodes, CallNodes} = elvis_code:find(#{
        of_types => [call],
        inside => elvis_code:root(Rule, ElvisConfig),
        filtered_by =>
            fun(CallNode) ->
                is_in_call_list(CallNode, NoCallFuns)
            end
    }),

    lists:map(
        fun(CallNode) ->
            {M, F, A} = call_mfa(CallNode),

            elvis_result:new_item(
                Msg,
                [M, F, A],
                #{node => CallNode}
            )
        end,
        CallNodes
    ).

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
