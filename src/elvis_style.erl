-module(elvis_style).
-behaviour(elvis_ruleset).

-export([
    default/1,
    function_naming_convention/3,
    variable_naming_convention/3,
    consistent_variable_casing/3,
    macro_names/3,
    no_macros/3,
    no_specs/3,
    no_types/3,
    no_nested_hrls/3,
    no_block_expressions/3,
    operator_spaces/3,
    no_space/3,
    no_space_after_pound/3,
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
    max_anonymous_function_arity/3,
    max_function_arity/3,
    max_function_length/3,
    max_function_clause_length/3,
    no_call/3,
    no_debug_call/3,
    no_common_caveats_call/3,
    no_nested_try_catch/3,
    no_successive_maps/3,
    atom_naming_convention/3,
    no_throw/3,
    no_dollar_space/3,
    no_author/3,
    no_import/3,
    no_catch_expressions/3,
    no_single_clause_case/3,
    no_single_match_maybe/3,
    numeric_format/3,
    behaviour_spelling/3,
    always_shortcircuit/3,
    consistent_generic_type/3,
    export_used_types/3,
    no_match_in_condition/3,
    param_pattern_matching/3,
    private_data_types/3,
    option/3,
    no_init_lists/3,
    ms_transform_included/3,
    no_boolean_in_comparison/3,
    no_operation_on_same_value/3,
    no_receive_without_timeout/3
]).

-export_type([empty_rule_config/0]).
-export_type([ignorable/0]).
-export_type([
    max_anonymous_function_arity_config/0,
    max_function_arity_config/0,
    max_function_length_config/0,
    max_module_length_config/0,
    function_naming_convention_config/0,
    variable_naming_convention_config/0,
    macro_names_config/0,
    no_macros_config/0,
    no_types_config/0,
    no_nested_hrls_config/0,
    no_specs_config/0,
    no_block_expressions_config/0,
    no_space_after_pound_config/0,
    operator_spaces_config/0,
    no_space_config/0,
    nesting_level_config/0,
    god_modules_config/0,
    module_naming_convention_config/0,
    dont_repeat_yourself_config/0,
    no_call_config/0,
    no_debug_call_config/0,
    no_common_caveats_call_config/0,
    atom_naming_convention_config/0,
    no_author_config/0,
    no_import_config/0,
    no_catch_expressions_config/0,
    numeric_format_config/0,
    no_single_clause_case_config/0,
    no_single_match_maybe_config/0,
    consistent_variable_casing_config/0,
    no_match_in_condition_config/0,
    behaviour_spelling_config/0,
    param_pattern_matching_config/0,
    private_data_type_config/0,
    no_init_lists_config/0,
    no_operation_on_same_value_config/0
]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Default values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec default(RuleName :: atom()) -> DefaultRuleConfig :: #{atom() := term()}.
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
default(RuleWithEmptyDefault) when
    RuleWithEmptyDefault =:= no_macros;
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

-type empty_rule_config() :: #{ignore => [ignorable()]}.
-type ignorable() :: module() | {module(), atom()} | {module(), atom(), arity()}.
-type max_function_length_config() ::
    #{
        ignore => [ignorable()],
        max_length => non_neg_integer(),
        count_comments => boolean(),
        count_whitespace => boolean()
    }.
-type max_module_length_config() ::
    #{
        ignore => [ignorable()],
        count_comments => boolean(),
        count_whitespace => boolean(),
        max_length => integer()
    }.
-type function_naming_convention_config() ::
    #{ignore => [ignorable()], regex => string()}.
-type binary_part() :: {Start :: non_neg_integer(), Length :: integer()}.

-spec function_naming_convention(
    elvis_config:config(),
    elvis_file:file(),
    function_naming_convention_config()
) ->
    [elvis_result:item()].
function_naming_convention(Config, Target, RuleConfig) ->
    Regex = option(regex, RuleConfig, function_naming_convention),
    ForbiddenRegex = option(forbidden_regex, RuleConfig, function_naming_convention),
    Root = get_root(Config, Target, RuleConfig),
    Functions = elvis_code:find_by_types([function], Root),
    errors_for_function_names(Regex, ForbiddenRegex, Functions).

errors_for_function_names(_Regex, _ForbiddenRegex, []) ->
    [];
errors_for_function_names(Regex, ForbiddenRegex, [Function | RemainingFunctions]) ->
    FunctionName = ktn_code:attr(name, Function),
    FunctionNameStr = unicode:characters_to_list(atom_to_list(FunctionName), unicode),
    case re:run(FunctionNameStr, Regex, [unicode]) of
        nomatch ->
            [
                elvis_result:new_item(
                    "the function ~p's name does not respect the format defined by the "
                    "regular expression '~p'",
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
                                    "the function ~p's name is written in a forbidden format"
                                    "defined by the regular expression '~p'",
                                    [FunctionNameStr, Regex],
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

-type consistent_variable_casing_config() :: #{ignore => [ignorable()]}.

-spec consistent_variable_casing(
    elvis_config:config(),
    elvis_file:file(),
    consistent_variable_casing_config()
) ->
    [elvis_result:item()].
%% @todo Use maps:groups_from_list/2 when we specify OTP25 as the minimum OTP version
consistent_variable_casing(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Vars = elvis_code:find(fun is_var/1, Root, #{traverse => all, mode => zipper}),
    Grouped =
        maps:to_list(
            lists:foldr(
                fun(Var, Acc) ->
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
                    "variable ~p (first used in line ~p) is written in "
                    "different ways within the module: ~p",
                    [FirstName, line(FirstVar), OtherNames],
                    #{node => FirstVar}
                )
            ]
    end.

-type variable_naming_convention_config() ::
    #{ignore => [ignorable()], regex => string()}.

-spec variable_naming_convention(
    elvis_config:config(),
    elvis_file:file(),
    variable_naming_convention_config()
) ->
    [elvis_result:item()].
variable_naming_convention(Config, Target, RuleConfig) ->
    Regex = option(regex, RuleConfig, variable_naming_convention),
    ForbiddenRegex = option(forbidden_regex, RuleConfig, variable_naming_convention),
    Root = get_root(Config, Target, RuleConfig),
    Vars = elvis_code:find(fun is_var/1, Root, #{traverse => all, mode => zipper}),
    check_variables_name(Regex, ForbiddenRegex, Vars).

-type macro_names_config() :: #{ignore => [ignorable()], regex => string()}.

-spec macro_names(elvis_config:config(), elvis_file:file(), macro_names_config()) ->
    [elvis_result:item()].
macro_names(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Regexp = option(regex, RuleConfig, macro_names),
    MacroNodes = elvis_code:find_by_types([define], Root, #{traverse => all, mode => node}),
    check_macro_names(Regexp, MacroNodes, _ResultsIn = []).

-type no_macros_config() :: #{allow => [atom()], ignore => [ignorable()]}.

-spec no_macros(elvis_config:config(), elvis_file:file(), no_macros_config()) ->
    [elvis_result:item()].
no_macros(ElvisConfig, RuleTarget, RuleConfig) ->
    TreeRootNode = get_root(ElvisConfig, RuleTarget, RuleConfig),
    AllowedMacros = maps:get(allow, RuleConfig, []) ++ eep_predef_macros() ++ logger_macros(),
    MacroNodes = elvis_code:find_by_types([macro], TreeRootNode),

    lists:foldl(
        fun(MacroNode, Acc) ->
            Macro = list_to_atom(ktn_code:attr(name, MacroNode)),
            case lists:member(Macro, AllowedMacros) of
                true ->
                    Acc;
                false ->
                    [
                        elvis_result:new_item(
                            "there is an unexpected macro (~p)",
                            [Macro],
                            #{node => MacroNode}
                        )
                        | Acc
                    ]
            end
        end,
        [],
        MacroNodes
    ).

-type no_types_config() :: #{allow => [atom()], ignore => [ignorable()]}.

-spec no_types(elvis_config:config(), elvis_file:file(), no_types_config()) ->
    [elvis_result:item()].
no_types(ElvisConfig, RuleTarget, RuleConfig) ->
    TreeRootNode = get_root(ElvisConfig, RuleTarget, RuleConfig),
    TypeNodes = elvis_code:find_by_types([type_attr], TreeRootNode),

    lists:foldl(
        fun(TypeNode, Acc) ->
            Type = ktn_code:attr(name, TypeNode),
            [
                elvis_result:new_item(
                    "there is an unexpected type (~p)",
                    [Type],
                    #{node => TypeNode}
                )
                | Acc
            ]
        end,
        [],
        TypeNodes
    ).

-type no_nested_hrls_config() :: #{allow => [atom()], ignore => [ignorable()]}.

-spec no_nested_hrls(elvis_config:config(), elvis_file:file(), no_nested_hrls_config()) ->
    [elvis_result:item()].
no_nested_hrls(ElvisConfig, RuleTarget, RuleConfig) ->
    TreeRootNode = get_root(ElvisConfig, RuleTarget, RuleConfig),
    IncludeNodes = elvis_code:find_by_types([include, include_lib], TreeRootNode),

    lists:foldl(
        fun(IncludeNode, Acc) ->
            Filename = ktn_code:attr(value, IncludeNode),
            [
                elvis_result:new_item(
                    "a nested include (~p) was found",
                    [Filename],
                    #{node => IncludeNode}
                )
                | Acc
            ]
        end,
        [],
        IncludeNodes
    ).

-type no_specs_config() :: #{allow => [atom()], ignore => [ignorable()]}.

-spec no_specs(elvis_config:config(), elvis_file:file(), no_specs_config()) ->
    [elvis_result:item()].
no_specs(ElvisConfig, RuleTarget, RuleConfig) ->
    TreeRootNode = get_root(ElvisConfig, RuleTarget, RuleConfig),
    SpecNodes = elvis_code:find_by_types([spec], TreeRootNode),

    lists:foldl(
        fun(SpecNode, Acc) ->
            FunctionName = ktn_code:attr(name, SpecNode),
            [
                elvis_result:new_item(
                    "there is an unexpected spec for function ~p",
                    [FunctionName],
                    #{node => SpecNode}
                )
                | Acc
            ]
        end,
        [],
        SpecNodes
    ).

-type no_block_expressions_config() :: #{ignore => [ignorable()]}.

-spec no_block_expressions(
    elvis_config:config(),
    elvis_file:file(),
    no_block_expressions_config()
) ->
    [elvis_result:item()].
no_block_expressions(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    BeginNodes = elvis_code:find_by_types_in_tokens(['begin'], Root),
    lists:foldl(
        fun(BeginNode, Acc) ->
            [
                elvis_result:new_item(
                    "there is an unexpected block expression (begin-end)",
                    #{node => BeginNode}
                )
                | Acc
            ]
        end,
        [],
        BeginNodes
    ).

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

-type no_space_after_pound_config() :: #{ignore => [ignorable()]}.

-spec no_space_after_pound(
    elvis_config:config(),
    elvis_file:file(),
    no_space_after_pound_config()
) ->
    [elvis_result:item()].
no_space_after_pound(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Tokens = ktn_code:attr(tokens, Root),
    TextNodes = lists:filter(fun is_text_node/1, Tokens),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src),
    check_spaces(Lines, TextNodes, {right, "#"}, Encoding, {should_not_have, []}).

-type operator_spaces_config() ::
    #{ignore => [ignorable()], rules => [{right | left, string()}]}.

punctuation_symbols() -> [',', ';', dot, '->', ':', '::', '|', '||'].

-spec operator_spaces(
    elvis_config:config(),
    elvis_file:file(),
    operator_spaces_config()
) ->
    [elvis_result:item()].
operator_spaces(Config, Target, RuleConfig) ->
    Rules = option(rules, RuleConfig, operator_spaces),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Root = get_root(Config, Target, RuleConfig),

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

-type no_space_config() ::
    #{ignore => [ignorable()], rules => [{right | left, string()}]}.

-spec no_space(elvis_config:config(), elvis_file:file(), no_space_config()) ->
    [elvis_result:item()].
no_space(Config, Target, RuleConfig) ->
    Rules = option(rules, RuleConfig, no_space),
    Root = get_root(Config, Target, RuleConfig),
    Tokens = ktn_code:attr(tokens, Root),
    TextNodes = lists:filter(fun is_text_node/1, Tokens),
    {Src, #{encoding := Encoding}} = elvis_file:src(Target),
    Lines = elvis_utils:split_all_lines(Src),
    AllSpaceUntilText =
        [
            {Text,
                re:compile(
                    "^[ ]+" ++
                        re:replace(
                            Text,
                            "(\\.|\\[|\\]|\\^|\\$|\\+|\\*|\\?|\\{|\\}|\\(|\\)|\\||\\\\)",
                            "\\\\\\1",
                            [{return, list}, global]
                        )
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

-type nesting_level_config() :: #{ignore => [ignorable()], level => integer()}.

-spec nesting_level(elvis_config:config(), elvis_file:file(), nesting_level_config()) ->
    [elvis_result:item()].
nesting_level(Config, Target, RuleConfig) ->
    Level = option(level, RuleConfig, nesting_level),

    Root = get_root(Config, Target, RuleConfig),

    elvis_utils:check_nodes(Root, fun check_nesting_level/2, [Level]).

-type god_modules_config() :: #{ignore => [ignorable()], limit => integer()}.

-spec god_modules(elvis_config:config(), elvis_file:file(), god_modules_config()) ->
    [elvis_result:item()].
god_modules(Config, Target, RuleConfig) ->
    Limit = option(limit, RuleConfig, god_modules),

    Root = get_root(Config, Target, RuleConfig),

    Exports = elvis_code:find_by_types([export], Root),
    Exported = lists:flatmap(fun(Node) -> ktn_code:attr(value, Node) end, Exports),
    case length(Exported) of
        Count when Count > Limit ->
            [
                elvis_result:new_item(
                    "This module has too many functions. Consider breaking it into several modules",
                    #{limit => Limit}
                )
            ];
        _ ->
            []
    end.

-spec no_if_expression(elvis_config:config(), elvis_file:file(), empty_rule_config()) ->
    [elvis_result:item()].
no_if_expression(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    case elvis_code:find_by_types(['if'], Root) of
        [] ->
            [];
        IfExprs ->
            ResultFun = fun(Node) ->
                elvis_result:new_item(
                    "an unexpected 'if' expression was found",
                    #{node => Node}
                )
            end,
            lists:map(ResultFun, IfExprs)
    end.

-spec invalid_dynamic_call(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
invalid_dynamic_call(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),

    case elvis_code:find_by_types([callback], Root) of
        [] ->
            check_invalid_dynamic_calls(Root);
        _Callbacks ->
            []
    end.

-spec used_ignored_variable(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
used_ignored_variable(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    ResultFun = fun(Node) ->
        elvis_result:new_item(
            "an unexpected use of an ignored variable was found",
            #{node => Node}
        )
    end,

    case elvis_code:find(fun is_ignored_var/1, Root, #{mode => zipper}) of
        [] ->
            [];
        UsedIgnoredVars ->
            lists:map(ResultFun, UsedIgnoredVars)
    end.

-spec no_behavior_info(elvis_config:config(), elvis_file:file(), empty_rule_config()) ->
    [elvis_result:item()].
no_behavior_info(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
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

    case lists:filter(FilterFun, Children) of
        [] ->
            [];
        BehaviorInfos ->
            ResultFun = fun(Node) ->
                elvis_result:new_item(
                    "an unexpected behaviour 'behavior_info/1' was found",
                    #{node => Node}
                )
            end,
            lists:map(ResultFun, BehaviorInfos)
    end.

-type module_naming_convention_config() :: #{ignore => [ignorable()], regex => string()}.

-spec module_naming_convention(
    elvis_config:config(),
    elvis_file:file(),
    module_naming_convention_config()
) ->
    [elvis_result:item()].
module_naming_convention(Config, Target, RuleConfig) ->
    Regex = option(regex, RuleConfig, module_naming_convention),
    ForbiddenRegex = option(forbidden_regex, RuleConfig, module_naming_convention),
    IgnoreModules = option(ignore, RuleConfig, module_naming_convention),

    Root = get_root(Config, Target, RuleConfig),
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
                            "The module's name does not respect the format defined by the "
                            "regular expression '~p'",
                            [Regex]
                        )
                    ];
                {match, _} ->
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
                    "The module's name is written in a forbidden format defined by the "
                    "regular expression '~p'",
                    [Regex]
                )
            ];
        nomatch ->
            []
    end.

-spec state_record_and_type(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
state_record_and_type(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    case is_otp_behaviour(Root) of
        true ->
            case {has_state_record(Root), has_state_type(Root)} of
                {true, true} ->
                    [];
                {false, _} ->
                    [
                        elvis_result:new_item(
                            "This module implements an OTP behavior but is missing a 'state' record"
                        )
                    ];
                {true, false} ->
                    [
                        elvis_result:new_item(
                            "This module implements an OTP behavior and has a 'state' record "
                            "but is missing a 'state()' type"
                        )
                    ]
            end;
        false ->
            []
    end.

-spec no_spec_with_records(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
no_spec_with_records(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    case elvis_code:find(fun spec_includes_record/1, Root) of
        [] ->
            [];
        SpecNodes ->
            ResultFun = fun(Node) ->
                elvis_result:new_item(
                    "there is an unexpected record in the spec",
                    #{node => Node}
                )
            end,
            lists:map(ResultFun, SpecNodes)
    end.

-type dont_repeat_yourself_config() ::
    #{ignore => [ignorable()], min_complexity => non_neg_integer()}.

-spec dont_repeat_yourself(
    elvis_config:config(),
    elvis_file:file(),
    dont_repeat_yourself_config()
) ->
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
                io_lib:format("~p, (~p, ~p)", [Str, Line, Col])
        end,
    ResultFun =
        fun(Locations) ->
            LocationsStr = lists:foldl(LocationCat, "", Locations),
            elvis_result:new_item(
                "The code in the following (LINE, COL) locations has the same structure: ~p",
                [LocationsStr]
            )
        end,

    lists:map(ResultFun, Nodes).

-spec max_module_length(
    elvis_config:config(),
    elvis_file:file(),
    max_module_length_config()
) ->
    [elvis_result:item()].
max_module_length(_Config, Target, RuleConfig) ->
    MaxLength = option(max_length, RuleConfig, max_module_length),
    CountComments = option(count_comments, RuleConfig, max_module_length),
    CountWhitespace = option(count_whitespace, RuleConfig, max_module_length),
    CountDocs = option(count_docs, RuleConfig, max_module_length),

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
                    "This module's lines-of-code count exceeds the allowed maximum",
                    #{limit => MaxLength}
                )
            ];
        _ ->
            []
    end.

-type max_anonymous_function_arity_config() :: #{max_arity => non_neg_integer()}.

-spec max_anonymous_function_arity(
    elvis_config:config(),
    elvis_file:file(),
    max_anonymous_function_arity_config()
) ->
    [elvis_result:item()].
max_anonymous_function_arity(Config, Target, RuleConfig) ->
    MaxArity = option(max_arity, RuleConfig, max_anonymous_function_arity),
    Root = get_root(Config, Target, RuleConfig),
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
                Arity ->
                    {true,
                        elvis_result:new_item(
                            "the arity (~p) of the anonymous function exceeds the limit",
                            [Arity],
                            #{node => Fun, limit => MaxArity}
                        )}
            end
        end,
        Funs
    ).

-type max_function_arity_config() ::
    #{max_arity => non_neg_integer(), non_exported_max_arity => pos_integer()}.

-spec max_function_arity(
    elvis_config:config(),
    elvis_file:file(),
    max_function_arity_config()
) ->
    [elvis_result:item()].
max_function_arity(Config, Target, RuleConfig) ->
    ExportedMaxArity = option(max_arity, RuleConfig, max_function_arity),
    NonExportedMaxArity =
        specific_or_default(
            option(non_exported_max_arity, RuleConfig, max_function_arity),
            ExportedMaxArity
        ),
    Root = get_root(Config, Target, RuleConfig),
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
                            "the arity of function ~p/~p exceeds the limit",
                            [Name, Arity],
                            #{node => Function, limit => MaxArity}
                        )}
            end
        end,
        Functions
    ).

-spec max_function_clause_length(
    elvis_config:config(),
    elvis_file:file(),
    max_function_length_config()
) ->
    [elvis_result:item()].
max_function_clause_length(Config, Target, RuleConfig) ->
    MaxLength = option(max_length, RuleConfig, max_function_length),
    CountComments = option(count_comments, RuleConfig, max_function_length),
    CountWhitespace = option(count_whitespace, RuleConfig, max_function_length),

    Root = get_root(Config, Target, RuleConfig),
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
                "the code for the ~p clause of function ~p/~p has ~p lines which exceeds the limit",
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

-spec max_function_length(
    elvis_config:config(),
    elvis_file:file(),
    max_function_length_config()
) ->
    [elvis_result:item()].
max_function_length(Config, Target, RuleConfig) ->
    MaxLength = option(max_length, RuleConfig, max_function_length),
    CountComments = option(count_comments, RuleConfig, max_function_length),
    CountWhitespace = option(count_whitespace, RuleConfig, max_function_length),

    Root = get_root(Config, Target, RuleConfig),
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
                "the code for function ~p/~p has ~p lines which exceeds the limit",
                [Name, Arity, L],
                #{line => StartPos, limit => MaxLength}
            )
        end,
    lists:map(ResultFun, FunLenMaxPairs).

-type function_spec() :: {module(), atom(), arity()} | {module(), atom()}.
-type no_call_config() ::
    #{ignore => [ignorable()], no_call_functions => [function_spec()]}.

-spec no_call(elvis_config:config(), elvis_file:file(), no_call_config()) ->
    [elvis_result:item()].
no_call(Config, Target, RuleConfig) ->
    DefaultFns = option(no_call_functions, RuleConfig, no_call),
    Msg = "there is an unexpected call to ~p:~p/~p (check no_call list)",
    no_call_common(Config, Target, DefaultFns, Msg, RuleConfig).

-type no_debug_call_config() ::
    #{ignore => [ignorable()], debug_functions => [function_spec()]}.

-spec no_debug_call(elvis_config:config(), elvis_file:file(), no_debug_call_config()) ->
    [elvis_result:item()].
no_debug_call(Config, Target, RuleConfig) ->
    DefaultFns = option(debug_functions, RuleConfig, no_debug_call),
    Msg = "there is an unexpected debug call to ~p:~p/~p",
    no_call_common(Config, Target, DefaultFns, Msg, RuleConfig).

-type no_common_caveats_call_config() ::
    #{ignore => [ignorable()], caveat_functions => [function_spec()]}.

-spec no_common_caveats_call(
    elvis_config:config(),
    elvis_file:file(),
    no_common_caveats_call_config()
) ->
    [elvis_result:item()].
no_common_caveats_call(Config, Target, RuleConfig) ->
    DefaultFns = option(caveat_functions, RuleConfig, no_common_caveats_call),
    Msg = "the call to ~p:~p/~p might have performance drawbacks or implicit behavior",
    no_call_common(Config, Target, DefaultFns, Msg, RuleConfig).

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

-spec no_nested_try_catch(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
no_nested_try_catch(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    ResultFun = fun(Node) ->
        elvis_result:new_item(
            "there is an unexpected nested try...catch block",
            #{node => Node}
        )
    end,
    case elvis_code:find_by_types(['try'], Root) of
        [] ->
            [];
        TryExprs ->
            lists:flatmap(fun(TryExp) -> check_nested_try_catchs(ResultFun, TryExp) end, TryExprs)
    end.

-spec no_successive_maps(elvis_config:config(), elvis_file:file(), empty_rule_config()) ->
    [elvis_result:item()].
no_successive_maps(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    ResultFun = fun(Node) ->
        elvis_result:new_item(
            "there is an unexpected map update after map construction/update",
            #{node => Node}
        )
    end,
    FindOpts = #{mode => node, traverse => all},
    case elvis_code:find_by_types([map], Root, FindOpts) of
        [] ->
            [];
        MapExprs ->
            lists:flatmap(fun(MapExp) -> check_successive_maps(ResultFun, MapExp) end, MapExprs)
    end.

-type atom_naming_convention_config() ::
    #{
        ignore => [ignorable()],
        regex => string(),
        enclosed_atoms => same | string()
    }.

-spec atom_naming_convention(
    elvis_config:config(),
    elvis_file:file(),
    atom_naming_convention_config()
) ->
    [elvis_result:item()].
atom_naming_convention(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Regex = option(regex, RuleConfig, atom_naming_convention),
    ForbiddenRegex = option(forbidden_regex, RuleConfig, atom_naming_convention),
    RegexEnclosed =
        specific_or_default(option(enclosed_atoms, RuleConfig, atom_naming_convention), Regex),
    ForbiddenEnclosedRegex =
        specific_or_default(
            option(forbidden_enclosed_regex, RuleConfig, atom_naming_convention),
            ForbiddenRegex
        ),
    IsAtomNode = fun(Node) ->
        ktn_code:type(zipper:node(Node)) =:= atom andalso not check_parent_remote(Node)
    end,
    AtomNodes = elvis_code:find(IsAtomNode, Root, #{traverse => all, mode => zipper}),
    check_atom_names(
        Regex,
        ForbiddenRegex,
        RegexEnclosed,
        ForbiddenEnclosedRegex,
        AtomNodes,
        []
    ).

-type no_init_lists_config() :: #{behaviours => [atom()]}.

-spec no_init_lists(elvis_config:config(), elvis_file:file(), no_init_lists_config()) ->
    [elvis_result:item()].
no_init_lists(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),

    ListInitClauses =
        case is_relevant_behaviour(Root, RuleConfig) of
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
                "an unexpected 'init' callback list argument was found",
                #{node => Node}
            )
        end,

    lists:map(ResultFun, ListInitClauses).

is_relevant_behaviour(Root, RuleConfig) ->
    ConfigBehaviors = option(behaviours, RuleConfig, no_init_lists),
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

-spec ms_transform_included(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
ms_transform_included(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),

    Functions = get_fun_2_ms_calls(Root),

    IsIncluded = Functions =/= [] andalso has_include_ms_transform(Root),

    ResultFun =
        fun(Function) ->
            elvis_result:new_item(
                "`ets:fun2ms/1` is used but the module is missing "
                "-include_lib(\"stdlib/include/ms_transform.hrl\")",
                #{node => Function}
            )
        end,

    case IsIncluded of
        true ->
            [];
        false ->
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

-spec no_boolean_in_comparison(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
no_boolean_in_comparison(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),

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
                "comparison uses boolean, which should be avoided",
                #{node => Node}
            )
        end,

    lists:map(ResultFun, ComparisonsWithBoolean).

-spec no_receive_without_timeout(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
no_receive_without_timeout(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),

    Receives = elvis_code:find_by_types(['receive'], Root),

    ReceivesWithoutTimeout = lists:filter(fun is_receive_without_timeout/1, Receives),

    ResultFun =
        fun(Node) ->
            elvis_result:new_item(
                "a receive block without an after clause was found",
                #{node => Node}
            )
        end,

    lists:map(ResultFun, ReceivesWithoutTimeout).

is_receive_without_timeout(Receive) ->
    [] == elvis_code:find_by_types([receive_after], Receive).

-type no_operation_on_same_value_config() :: #{operations := [atom()]}.

-spec no_operation_on_same_value(
    elvis_config:config(),
    elvis_file:file(),
    no_operation_on_same_value_config()
) ->
    [elvis_result:item()].
no_operation_on_same_value(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    InterestingOps = option(operations, RuleConfig, no_operation_on_same_value),

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
                "operation ~p has the same value on both sides, which is redundant",
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

-spec no_throw(elvis_config:config(), elvis_file:file(), empty_rule_config()) ->
    [elvis_result:item()].
no_throw(Config, Target, RuleConfig) ->
    Zipper =
        fun(Node) ->
            lists:any(fun(T) -> is_call(Node, T) end, [{throw, 1}, {erlang, throw, 1}])
        end,
    Root = get_root(Config, Target, RuleConfig),
    ThrowNodes = elvis_code:find(Zipper, Root),
    lists:foldl(
        fun(ThrowNode, AccIn) ->
            [
                elvis_result:new_item(
                    "usage of throw/1 is not recommended",
                    #{node => ThrowNode}
                )
                | AccIn
            ]
        end,
        [],
        ThrowNodes
    ).

-spec no_dollar_space(elvis_config:config(), elvis_file:file(), empty_rule_config()) ->
    [elvis_result:item()].
no_dollar_space(Config, Target, RuleConfig) ->
    IsDollarSpace =
        fun(Node) -> ktn_code:type(Node) =:= char andalso ktn_code:attr(text, Node) =:= "$ " end,
    Root = get_root(Config, Target, RuleConfig),
    Opts = #{mode => node, traverse => all},
    DollarSpaceNodes = elvis_code:find(IsDollarSpace, Root, Opts),
    lists:map(
        fun(ThrowNode) ->
            elvis_result:new_item(
                "an unexpected '$ ' was found. Use $\\s, instead",
                #{node => ThrowNode}
            )
        end,
        DollarSpaceNodes
    ).

-type no_author_config() :: #{ignore => [ignorable()]}.

-spec no_author(elvis_config:config(), elvis_file:file(), no_author_config()) ->
    [elvis_result:item()].
no_author(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Nodes = elvis_code:find_by_types([author], Root),
    lists:map(
        fun(Node) ->
            elvis_result:new_item(
                "an unexpected -author attribute was found",
                #{node => Node}
            )
        end,
        Nodes
    ).

-type no_import_config() :: #{ignore => [ignorable()]}.

-spec no_import(elvis_config:config(), elvis_file:file(), no_import_config()) ->
    [elvis_result:item()].
no_import(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Nodes = elvis_code:find_by_types([import], Root),
    lists:map(
        fun(Node) ->
            elvis_result:new_item(
                "an unexpected -import attribute was found",
                #{node => Node}
            )
        end,
        Nodes
    ).

-type no_catch_expressions_config() :: #{ignore => [ignorable()]}.

-spec no_catch_expressions(
    elvis_config:config(),
    elvis_file:file(),
    no_catch_expressions_config()
) ->
    [elvis_result:item()].
no_catch_expressions(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    CatchNodes = elvis_code:find_by_types(['catch'], Root),
    lists:foldl(
        fun(CatchNode, Acc) ->
            [
                elvis_result:new_item(
                    "an unexpected catch expression was found",
                    #{node => CatchNode}
                )
                | Acc
            ]
        end,
        [],
        CatchNodes
    ).

-type no_single_clause_case_config() :: #{ignore => [ignorable()]}.

-spec no_single_clause_case(
    elvis_config:config(),
    elvis_file:file(),
    no_single_clause_case_config()
) ->
    [elvis_result:item()].
no_single_clause_case(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    IsSingleClauseCaseStatement = fun(Node) ->
        ktn_code:type(Node) =:= 'case' andalso length(case_clauses_in(Node)) =:= 1
    end,
    CaseNodes = elvis_code:find(IsSingleClauseCaseStatement, Root),
    lists:map(
        fun(CaseNode) ->
            elvis_result:new_item(
                "an unexpected single-clause case expression was found",
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

-type no_single_match_maybe_config() :: #{ignore => [ignorable()]}.

-spec no_single_match_maybe(
    elvis_config:config(),
    elvis_file:file(),
    no_single_match_maybe_config()
) ->
    [elvis_result:item()].
no_single_match_maybe(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    IsSingleMatchMaybeStatement = fun(Node) ->
        ktn_code:type(Node) =:= 'maybe' andalso length(ktn_code:content(Node)) =:= 1
    end,
    CaseNodes = elvis_code:find(IsSingleMatchMaybeStatement, Root),
    lists:map(
        fun(CaseNode) ->
            elvis_result:new_item(
                "an unexpected single-match maybe statement was found",
                #{node => CaseNode}
            )
        end,
        CaseNodes
    ).

-type no_match_in_condition_config() :: #{ignore => [ignorable()]}.

-spec no_match_in_condition(
    elvis_config:config(),
    elvis_file:file(),
    no_match_in_condition_config()
) ->
    [elvis_result:item()].
no_match_in_condition(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    CaseNodes = elvis_code:find(fun is_match_in_condition/1, Root),
    lists:map(
        fun(CaseNode) ->
            elvis_result:new_item(
                "an unexpected match condition was found in a case statement",
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

-type numeric_format_config() ::
    #{
        ignore => [ignorable()],
        regex => string(),
        int_regex => same | string(),
        float_regex => same | string()
    }.

-spec numeric_format(elvis_config:config(), elvis_file:file(), numeric_format_config()) ->
    [elvis_result:item()].
numeric_format(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
    Regex = option(regex, RuleConfig, numeric_format),
    IntRegex = specific_or_default(option(int_regex, RuleConfig, numeric_format), Regex),
    FloatRegex = specific_or_default(option(float_regex, RuleConfig, numeric_format), Regex),
    IntNodes = elvis_code:find_by_types([integer], Root),
    FloatNodes = elvis_code:find_by_types([float], Root),
    check_numeric_format(
        IntRegex,
        IntNodes,
        check_numeric_format(FloatRegex, FloatNodes, [])
    ).

-type behaviour_spelling_config() ::
    #{ignore => [ignorable()], spelling => behaviour | behavior}.

-spec behaviour_spelling(
    elvis_config:config(),
    elvis_file:file(),
    behaviour_spelling_config()
) ->
    [elvis_result:item()].
behaviour_spelling(Config, Target, RuleConfig) ->
    Spelling = option(spelling, RuleConfig, behaviour_spelling),
    Root = get_root(Config, Target, RuleConfig),
    IsWronglySpelledBehaviour =
        fun(Node) ->
            (ktn_code:type(Node) =:= behaviour orelse ktn_code:type(Node) =:= behavior) andalso
                ktn_code:type(Node) =/= Spelling
        end,
    case elvis_code:find(IsWronglySpelledBehaviour, Root) of
        [] ->
            [];
        InconsistentBehaviorNodes ->
            ResultFun =
                fun(Node) ->
                    elvis_result:new_item(
                        "an unexpected spelling of behavior/behaviour was found; prefer ~p",
                        [Spelling],
                        #{node => Node}
                    )
                end,
            lists:map(ResultFun, InconsistentBehaviorNodes)
    end.

-type param_pattern_matching_config() :: #{ignore => [ignorable()], side => left | right}.

-spec param_pattern_matching(
    elvis_config:config(),
    elvis_file:file(),
    param_pattern_matching_config()
) ->
    [elvis_result:item()].
param_pattern_matching(Config, Target, RuleConfig) ->
    Side = option(side, RuleConfig, param_pattern_matching),
    Root = get_root(Config, Target, RuleConfig),

    FunctionClausePatterns =
        lists:flatmap(
            fun(Clause) -> ktn_code:node_attr(pattern, Clause) end,
            elvis_code:find(
                fun is_function_clause/1,
                Root,
                #{mode => zipper, traverse => all}
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
                            "variable ~p, used to match an argument, is placed on "
                            "the wrong side of the match; prefer the ~p side",
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

-spec consistent_generic_type(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
consistent_generic_type(Config, Target, RuleConfig) ->
    TypePreference = option(preferred_type, RuleConfig, consistent_generic_type),
    Root = get_root(Config, Target, RuleConfig),
    IsInconsistentGenType = consistent_generic_type_predicate(TypePreference),
    case elvis_code:find(IsInconsistentGenType, Root, #{traverse => all, mode => node}) of
        [] ->
            [];
        InconsistentTypeNodes ->
            ResultFun = consistent_generic_type_result(TypePreference),
            lists:map(ResultFun, InconsistentTypeNodes)
    end.

-spec always_shortcircuit(
    elvis_config:config(),
    elvis_file:file(),
    empty_rule_config()
) ->
    [elvis_result:item()].
always_shortcircuit(Config, Target, RuleConfig) ->
    Operators = #{'and' => 'andalso', 'or' => 'orelse'},
    Root = get_root(Config, Target, RuleConfig),
    IsBadOperator =
        fun(Node) ->
            is_operator_node(Node) andalso
                lists:member(
                    ktn_code:attr(operation, Node), maps:keys(Operators)
                )
        end,
    case elvis_code:find(IsBadOperator, Root, #{traverse => all}) of
        [] ->
            [];
        BadOperators ->
            ResultFun =
                fun(Node) ->
                    BadOperator = ktn_code:attr(operation, Node),
                    GoodOperator = maps:get(BadOperator, Operators),
                    elvis_result:new_item(
                        "an unexpected non-shortcircuiting operator (~p) was found; prefer ~p",
                        [BadOperator, GoodOperator],
                        #{node => Node}
                    )
                end,
            lists:map(ResultFun, BadOperators)
    end.

-spec export_used_types(elvis_config:config(), elvis_file:file(), empty_rule_config()) ->
    [elvis_result:item()].
export_used_types(Config, Target, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),
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
                    Types = elvis_code:find_by_types([user_type], Spec, #{
                        mode => node, traverse => all
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
                "type ~p/~p is used by an exported function but it is not exported itself",
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

-type data_type() :: record | map | tuple.
-type private_data_type_config() :: #{apply_to => [data_type()]}.

-spec private_data_types(
    elvis_config:config(),
    elvis_file:file(),
    private_data_type_config()
) ->
    [elvis_result:item()].
private_data_types(Config, Target, RuleConfig) ->
    TypesToCheck = option(apply_to, RuleConfig, private_data_types),
    TreeRootNode = get_root(Config, Target, RuleConfig),
    TypeExports = elvis_code:find_by_types([export_type], TreeRootNode),
    ExportedTypes = lists:flatmap(fun(Node) -> ktn_code:attr(value, Node) end, TypeExports),
    Locations = map_type_declarations_to_location(TreeRootNode),

    PublicDataTypes = public_data_types(TypesToCheck, TreeRootNode, ExportedTypes),

    lists:map(
        fun({Name, Arity} = Info) ->
            {Line, Column} = maps:get(Info, Locations, {-1, -1}),
            elvis_result:new_item(
                "private data type ~p/~p is exported; prefer not exporting it or making it opaque",
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
         || Node <- elvis_code:find(Fun, TreeRootNode, #{traverse => all, mode => node})
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
                                "number ~p does not respect the format "
                                "defined by regular expression '~p'",
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

check_atom_names(_Regex, _, _RegexEnclosed, _, [] = _AtomNodes, Acc) ->
    Acc;
check_atom_names(
    Regex,
    ForbiddenRegex,
    RegexEnclosed,
    ForbiddenRegexEnclosed,
    [AtomNode | RemainingAtomNodes],
    AccIn
) ->
    AtomName0 = ktn_code:attr(text, AtomNode),
    ValueAtomName = ktn_code:attr(value, AtomNode),
    {IsEnclosed, AtomName} = string_strip_enclosed(AtomName0),
    IsExceptionClass = is_exception_or_non_reversible(ValueAtomName),
    RE = re_compile_for_atom_type(IsEnclosed, Regex, RegexEnclosed),
    REF = re_compile_for_atom_type(IsEnclosed, ForbiddenRegex, ForbiddenRegexEnclosed),
    AtomNameUnicode = unicode:characters_to_list(AtomName, unicode),
    AccOut =
        case re:run(AtomNameUnicode, RE) of
            _ when IsExceptionClass, not IsEnclosed ->
                [];
            nomatch when not IsEnclosed ->
                [
                    elvis_result:new_item(
                        "atom ~p's name does not respect the format defined by "
                        "regular expression '~p'",
                        [AtomName0, Regex],
                        #{node => AtomNode}
                    )
                ];
            nomatch when IsEnclosed ->
                [
                    elvis_result:new_item(
                        "enclosed atom ~p's name does not respect the format defined by "
                        "regular expression '~p'",
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
                                "atom ~p's name is written in a forbidden format "
                                "defined by the regular expression '~p'",
                                [AtomName, ForbiddenRegex],
                                #{node => AtomNode}
                            )
                        ];
                    {match, _} when IsEnclosed ->
                        [
                            elvis_result:new_item(
                                "enclosed atom ~p's name is written in a forbidden format "
                                "defined by the regular expression '~p'",
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
        RemainingAtomNodes,
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
    {ok, RE} = re:compile(Regex, [unicode]),
    RE;
re_compile_for_atom_type(true = _IsEnclosed, _Regex, RegexEnclosed) ->
    {ok, RE} = re:compile(RegexEnclosed, [unicode]),
    RE.

%% Variables name
check_variables_name(_Regex, _, []) ->
    [];
check_variables_name(Regex, ForbiddenRegex, [Variable | RemainingVars]) ->
    VariableNameStr = atom_to_list(ktn_code:attr(name, Variable)),
    case re:run(VariableNameStr, Regex) of
        nomatch when VariableNameStr =:= "_" ->
            check_variables_name(Regex, ForbiddenRegex, RemainingVars);
        nomatch ->
            [
                elvis_result:new_item(
                    "variable ~p's name does not respect the format "
                    "defined by the regular expression '~p'",
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
                                    "variable ~p's name is written in a format forbidden by the "
                                    "specified expression '~p'",
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

check_macro_names(_Regexp, [] = _MacroNodes, ResultsIn) ->
    ResultsIn;
check_macro_names(Regexp, [MacroNode | RemainingMacroNodes], ResultsIn) ->
    {ok, RE} = re:compile(Regexp, [unicode]),
    {MacroNameStripped0, MacroNameOriginal} = macro_name_from_node(MacroNode),
    MacroNameStripped = unicode:characters_to_list(MacroNameStripped0, unicode),
    ResultsOut =
        case re:run(MacroNameStripped, RE) of
            nomatch ->
                [
                    elvis_result:new_item(
                        "the macro named ~p does not respect the format "
                        "defined by regular expression '~p'",
                        [MacroNameOriginal, Regexp],
                        #{node => MacroNode}
                    )
                    | ResultsIn
                ];
            {match, _Captured} ->
                ResultsIn
        end,
    check_macro_names(Regexp, RemainingMacroNodes, ResultsOut).

macro_name_from_node(MacroNode) ->
    MacroNodeValue = ktn_code:attr(value, MacroNode),
    MacroAsAtom = macro_as_atom(false, [call, var, atom], MacroNodeValue),
    MacroNameOriginal = atom_to_list(MacroAsAtom),
    MacroNameStripped = string:strip(MacroNameOriginal, both, $'),
    {MacroNameStripped, MacroNameOriginal}.

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
                            "there is a missing space to the ~p of ~p",
                            [Position, Text],
                            #{node => Node}
                        )
                    ];
                _ when How0 =:= should_not_have ->
                    [
                        elvis_result:new_item(
                            "there is an unexpected space to the ~p of ~p",
                            [Position, Text],
                            #{node => Node}
                        )
                    ]
            end
        end,
    lists:flatmap(FlatFun, Nodes).

maybe_run_regex(undefined = _Regex, _Line) ->
    nomatch;
maybe_run_regex({ok, Regex}, Line) ->
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
                maybe_run_regex(proplists:get_value(Text, TextRegexes), Line)
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
    case past_nesting_limit(ParentNode, MaxLevel) of
        [] ->
            [];
        NestedNodes ->
            Fun = fun(Node) ->
                elvis_result:new_item(
                    "the expression is nested beyond the configured limit",
                    #{node => Node, limit => MaxLevel}
                )
            end,

            lists:map(Fun, NestedNodes)
    end.

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
    case elvis_code:find(fun is_dynamic_call/1, Root, #{traverse => all}) of
        [] ->
            [];
        InvalidCalls ->
            ResultFun = fun(Node) ->
                elvis_result:new_item(
                    "there is an unexpected dynamic function call. "
                    "Only modules that define callbacks should make dynamic calls",
                    #{node => Node}
                )
            end,
            lists:map(ResultFun, InvalidCalls)
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
    Opts = #{traverse => all},
    (ktn_code:type(Node) =:= spec) andalso (elvis_code:find(IsTypeRecord, Node, Opts) =/= []).

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
-spec no_call_common(
    elvis_config:config(),
    elvis_file:file(),
    [function_spec()],
    string(),
    RuleConfig :: elvis_core:rule_config()
) ->
    [elvis_result:item()].
no_call_common(Config, Target, NoCallFuns, Msg, RuleConfig) ->
    Root = get_root(Config, Target, RuleConfig),

    Calls = elvis_code:find_by_types([call], Root),
    check_no_call(Calls, Msg, NoCallFuns).

-spec check_no_call([ktn_code:tree_node()], string(), [function_spec()]) ->
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
            "an unexpected usage of type ~p/0 was found; prefer ~p/0",
            [NodeName, TypePreference],
            #{node => Node}
        )
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function Definitions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec option(OptionName, RuleConfig, Rule) -> OptionValue when
    OptionName :: atom(),
    RuleConfig :: elvis_core:rule_config(),
    Rule :: atom(),
    OptionValue :: term().
option(OptionName, RuleConfig, Rule) ->
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

-spec get_root(Config, Target, RuleConfig) -> Res when
    Config :: elvis_config:config(),
    Target :: elvis_file:file(),
    RuleConfig :: (Options :: #{atom() => term()}),
    Res :: ktn_code:tree_node().
get_root(Config, Target, RuleConfig) ->
    {Root0, File0} = elvis_file:parse_tree(Config, Target, RuleConfig),
    case maps:get(ruleset, Config, undefined) of
        Ruleset when Ruleset =:= beam_files; Ruleset =:= beam_files_strict ->
            maps:get(abstract_parse_tree, File0);
        _ ->
            Root0
    end.

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
