-module(style_SUITE).

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1
        ]).

-export([
         verify_function_naming_convention/1,
         verify_variable_naming_convention/1,
         verify_line_length_rule/1,
         verify_line_length_rule_latin1/1,
         verify_unicode_line_length_rule/1,
         verify_no_tabs_rule/1,
         verify_no_spaces_rule/1,
         verify_no_trailing_whitespace_rule/1,
         verify_macro_names_rule/1,
         verify_macro_module_names/1,
         verify_operator_spaces/1,
         verify_operator_spaces_latin1/1,
         verify_nesting_level/1,
         verify_god_modules/1,
         verify_no_if_expression/1,
         verify_invalid_dynamic_call/1,
         verify_used_ignored_variable/1,
         verify_no_behavior_info/1,
         verify_module_naming_convention/1,
         verify_state_record_and_type/1,
         verify_no_spec_with_records/1,
         verify_dont_repeat_yourself/1,
         verify_max_module_length/1,
         verify_max_function_length/1,
         verify_no_debug_call/1,
         verify_no_common_caveats_call/1,
         verify_no_call/1,
         verify_no_nested_try_catch/1,
         verify_atom_naming_convention/1,
         %% -elvis attribute
         verify_elvis_attr_atom_naming_convention/1,
         verify_elvis_attr_dont_repeat_yourself/1,
         verify_elvis_attr_function_naming_convention/1,
         verify_elvis_attr_god_modules/1,
         verify_elvis_attr_invalid_dynamic_call/1,
         verify_elvis_attr_line_length/1,
         verify_elvis_attr_macro_module_names/1,
         verify_elvis_attr_macro_names/1,
         verify_elvis_attr_max_function_length/1,
         verify_elvis_attr_max_module_length/1,
         verify_elvis_attr_module_naming_convention/1,
         verify_elvis_attr_nesting_level/1,
         verify_elvis_attr_no_behavior_info/1,
         verify_elvis_attr_no_call/1,
         verify_elvis_attr_no_debug_call/1,
         verify_elvis_attr_no_if_expression/1,
         verify_elvis_attr_no_nested_try_catch/1,
         verify_elvis_attr_no_spaces/1,
         verify_elvis_attr_no_spec_with_records/1,
         verify_elvis_attr_no_tabs/1,
         verify_elvis_attr_no_trailing_whitespace/1,
         verify_elvis_attr_operator_spaces/1,
         verify_elvis_attr_state_record_and_type/1,
         verify_elvis_attr_used_ignored_variable/1,
         verify_elvis_attr_variable_naming_convention/1,
         %% Non-rule
         results_are_ordered_by_line/1
        ]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite
        ]).

-type config() :: [{atom(), term()}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    _ = application:ensure_all_started(elvis_core),
    Config.

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
    ok = application:stop(elvis_core),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test Cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%%% Rules

-spec verify_function_naming_convention(config()) -> any().
verify_function_naming_convention(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    % pass
    PathPass = "pass_function_naming_convention.erl",
    {ok, FilePass} = elvis_test_utils:find_file(SrcDirs, PathPass),


    RuleConfig = #{regex => "^([a-z][a-z0-9]*_?)*$"},
    [] = elvis_style:function_naming_convention(ElvisConfig, FilePass, RuleConfig),

    RuleConfig2 = #{regex => "^([a-z][a-z0-9]*_?)*$",
                    ignore => [fail_function_naming_convention]
                   },
    [] = elvis_style:function_naming_convention(ElvisConfig, FilePass, RuleConfig2),

    % fail
    PathFail = "fail_function_naming_convention.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),

    [_CamelCaseError, _ALL_CAPSError, _InitialCapError,
     _HyphenError, _PredError, _EmailError] =
      elvis_style:function_naming_convention(ElvisConfig, FileFail, RuleConfig),

    RuleConfig3 = #{regex => "^([a-z][a-z0-9]*_?)*$",
                    ignore => [ {fail_function_naming_convention, camelCase}
                              , {fail_function_naming_convention, 'ALL_CAPS'}
                              , {fail_function_naming_convention, 'Initial_cap'}
                              , {fail_function_naming_convention, 'ok-for-lisp'}
                              , {fail_function_naming_convention, 'no_predicates?'}
                              ]
                   },
    [_EmailError] = elvis_style:function_naming_convention(ElvisConfig, FileFail, RuleConfig3),

    % ignored
    PathIgnored = "fail_function_naming_convention_ignored_function.erl",
    {ok, FileIgnored} = elvis_test_utils:find_file(SrcDirs, PathIgnored),

    RuleConfig4 = #{regex => "^([a-z][a-z0-9]*_?)*$",
                    ignore => [ {fail_function_naming_convention, camelCase}
                              , {fail_function_naming_convention, 'ALL_CAPS'}
                              , {fail_function_naming_convention, 'Initial_cap'}
                              , {fail_function_naming_convention, 'ok-for-lisp'}
                              , {fail_function_naming_convention, 'no_predicates?'}
                              , {fail_function_naming_convention, user@location}
                              ]
                   },
    [_AnError] = elvis_style:function_naming_convention(ElvisConfig, FileIgnored, RuleConfig4).

-spec verify_variable_naming_convention(config()) -> any().
verify_variable_naming_convention(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    RuleConfig = #{regex => "^_?([A-Z][0-9a-zA-Z]*)$"},

    PathPass = "pass_variable_naming_convention.erl",
    {ok, FilePass} = elvis_test_utils:find_file(SrcDirs, PathPass),
    [] = elvis_style:variable_naming_convention(ElvisConfig,
                                                FilePass,
                                                RuleConfig),

    PathFail = "fail_variable_naming_convention.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),
    [_AtSign,
     _Underline_Word_Separator,
     _Bad_Ignored_Variable,
     _AtSignAgain,
     _Underline_Word_SeparatorAgain] =
        elvis_style:variable_naming_convention(ElvisConfig,
                                               FileFail,
                                               RuleConfig).

-spec verify_line_length_rule(config()) -> any().
verify_line_length_rule(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    File = "fail_line_length.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    Result = elvis_style:line_length(ElvisConfig, Path, #{limit => 100}),
    8 = length(Result),
    #{info := Info, message := Msg} = lists:nth(7, Result),
    <<"Line 34 is too long:     gb_trees:from_orddict(", _/binary>> =
        list_to_binary(io_lib:format(Msg, Info)),

    WholeLineResult = elvis_style:line_length(ElvisConfig, Path,
                                              #{limit => 100,
                                                skip_comments => whole_line}),
    6 = length(WholeLineResult),

    AnyResult = elvis_style:line_length(ElvisConfig, Path,
                                        #{limit => 100,
                                          skip_comments => any}),
    6 = length(AnyResult).

-spec verify_line_length_rule_latin1(config()) -> any().
verify_line_length_rule_latin1(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    File = "fail_line_length_latin1.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    Result = elvis_style:line_length(ElvisConfig, Path, #{limit => 100}),
    1 = length(Result),
    #{info := Info, message := Msg} = lists:nth(1, Result),
    <<"Line 13 is too long:", _/binary>> = list_to_binary(io_lib:format(Msg, Info)).

-spec verify_unicode_line_length_rule(config()) -> any().
verify_unicode_line_length_rule(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Pass = "pass_unicode_comments.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, Pass),

    Result = elvis_style:line_length(ElvisConfig, Path, #{limit => 100}),
    0 = length(Result).

-spec verify_no_tabs_rule(config()) -> any().
verify_no_tabs_rule(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    File = "fail_no_tabs.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    [_, _] = elvis_style:no_tabs(ElvisConfig, Path, #{}).

-spec verify_no_spaces_rule(config()) -> any().
verify_no_spaces_rule(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    File = "fail_no_spaces.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    [_, _, _, _, _, _] = elvis_style:no_spaces(ElvisConfig, Path, #{}).

-spec verify_no_trailing_whitespace_rule(config()) -> any().
verify_no_trailing_whitespace_rule(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    File = "fail_no_trailing_whitespace.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    do_verify_no_trailing_whitespace(Path, ElvisConfig,
                                     #{ignore_empty_lines => true}, 3),
    do_verify_no_trailing_whitespace(Path, ElvisConfig,
                                     #{ignore_empty_lines => false}, 4),
    do_verify_no_trailing_whitespace(Path, ElvisConfig, #{}, 4).

do_verify_no_trailing_whitespace(Path, Config, RuleConfig, ExpectedNumItems) ->
    Items = elvis_style:no_trailing_whitespace(Config, Path, RuleConfig),
    length(Items) == ExpectedNumItems orelse
        ct:fail("Expected ~b error items. Got: ~p", [ExpectedNumItems, Items]).

-spec verify_macro_names_rule(config()) -> any().
verify_macro_names_rule(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    File = "fail_macro_names.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    [_, _, _, _] = elvis_style:macro_names(ElvisConfig, Path, #{}),

    [_, _] = elvis_style:macro_names(ElvisConfig, Path, #{ regex => "^[A-Za-z_ ]+$" }),

    [_] = elvis_style:macro_names(ElvisConfig, Path, #{ regex => "^[A-Za-z_ \-]+$" }),

    [] = elvis_style:macro_names(ElvisConfig, Path, #{ regex => "^[A-Za-z_, \-]+$" }),

    [_, _, _, _, _, _] = elvis_style:macro_names(ElvisConfig, Path, #{ regex => "^POTENTIAL_BAD-NAME$" }),

    [] = elvis_style:macro_names(ElvisConfig, Path, #{ ignore => [fail_macro_names] }).

-spec verify_macro_module_names(config()) -> any().
verify_macro_module_names(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    File = "fail_macro_module_names.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    [_, _, _, _] = elvis_style:macro_module_names(ElvisConfig, Path, #{}).

-spec verify_operator_spaces(config()) -> any().
verify_operator_spaces(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    File = "fail_operator_spaces.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    [] = elvis_style:operator_spaces(ElvisConfig, Path, #{rules=>[]}),

    RuleConfig = #{rules => [{right, ","}]},
    [_, _, _] = elvis_style:operator_spaces(ElvisConfig, Path, RuleConfig),

    AppendOptions = #{rules => [{right, "++"}, {left, "++"}]},
    [_] = elvis_style:operator_spaces(ElvisConfig, Path, AppendOptions),

    SumOperation = #{rules => [{right, "+"}, {left, "+"}]},
    [_, _] = elvis_style:operator_spaces(ElvisConfig, Path, SumOperation),

    MinusOperation = #{rules => [{right, "-"}, {left, "-"}]},
    [] = elvis_style:operator_spaces(ElvisConfig, Path, MinusOperation),

    AllOptions = #{rules => [{right, ","},
                             {right, "++"},
                             {left, "++"},
                             {right, "+"},
                             {left, "+"}]},
    [_, _, _, _, _, _] =
        elvis_style:operator_spaces(ElvisConfig, Path, AllOptions).

-spec verify_operator_spaces_latin1(config()) -> any().
verify_operator_spaces_latin1(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    File = "fail_operator_spaces_latin1.erl",
    {ok, Path} = elvis_test_utils:find_file(SrcDirs, File),

    [] = elvis_style:operator_spaces(ElvisConfig, Path, #{rules => []}),

    AppendOptions = #{rules => [{right, "++"}, {left, "++"}]},
    [_, _] = elvis_style:operator_spaces(ElvisConfig, Path, AppendOptions).


-spec verify_nesting_level(config()) -> any().
verify_nesting_level(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Path = "fail_nesting_level.erl",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Path),

    [ #{line_num := 11}
    , #{line_num := 18}
    , #{line_num := 30}
    , #{line_num := 45}
    , #{line_num := 78}
    , #{line_num := 120}
    , #{line_num := 166}
    , #{line_num := 182}
    ] = elvis_style:nesting_level(ElvisConfig, File, #{level => 3}),
    [] = elvis_style:nesting_level( ElvisConfig
                                  , File
                                  , #{ignore => [fail_nesting_level]}).

-spec verify_god_modules(config()) -> any().
verify_god_modules(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Path = "fail_god_modules.erl",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Path),
    [_] = elvis_style:god_modules(ElvisConfig, File, #{limit => 25}),

    RuleConfig = #{limit => 25, ignore => [fail_god_modules]},
    [] = elvis_style:god_modules(ElvisConfig, File, RuleConfig).

-spec verify_no_if_expression(config()) -> any().
verify_no_if_expression(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Path = "fail_no_if_expression.erl",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Path),
    [
     #{line_num := 11},
     #{line_num := 22},
     #{line_num := 31}
    ] = elvis_style:no_if_expression(ElvisConfig, File, #{}).

-spec verify_invalid_dynamic_call(config()) -> any().
verify_invalid_dynamic_call(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    PathPass = "pass_invalid_dynamic_call.erl",
    {ok, FilePass} = elvis_test_utils:find_file(SrcDirs, PathPass),
    [] = elvis_style:invalid_dynamic_call(ElvisConfig, FilePass, #{}),

    PathFail = "fail_invalid_dynamic_call.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),
    [
     #{line_num := 21},
     #{line_num := 33},
     #{line_num := 34},
     #{line_num := 42},
     #{line_num := 50},
     #{line_num := 61},
     #{line_num := 68}
    ] = elvis_style:invalid_dynamic_call(ElvisConfig, FileFail, #{}),

    RuleConfig = #{ignore => [fail_invalid_dynamic_call]},
    [] = elvis_style:invalid_dynamic_call(ElvisConfig,
                                          FileFail,
                                          RuleConfig).

-spec verify_used_ignored_variable(config()) -> any().
verify_used_ignored_variable(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Path = "fail_used_ignored_variable.erl",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Path),
    [
     #{line_num := 10},
     #{line_num := 13},
     #{line_num := 17},
     #{line_num := 17}
    ] = elvis_style:used_ignored_variable(ElvisConfig, File, #{}),
    [] = elvis_style:used_ignored_variable(ElvisConfig,
                                           File,
                                           #{ignore => [fail_used_ignored_variable]}).

-spec verify_no_behavior_info(config()) -> any().
verify_no_behavior_info(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    Path = "fail_no_behavior_info.erl",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Path),
    [
     #{line_num := 14},
     #{line_num := 17}
    ] = elvis_style:no_behavior_info(ElvisConfig, File, #{}).

-spec verify_module_naming_convention(config()) -> any().
verify_module_naming_convention(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    RuleConfig = #{regex => "^([a-z][a-z0-9]*_?)*$",
                   ignore => []},

    PathPass = "pass_module_naming_convention.erl",
    {ok, FilePass} = elvis_test_utils:find_file(SrcDirs, PathPass),
    [] =
        elvis_style:module_naming_convention(ElvisConfig, FilePass, RuleConfig),

    PathFail = "fail_module_naming_1_convention_1.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),
    [_] =
        elvis_style:module_naming_convention(ElvisConfig, FileFail, RuleConfig),

    RuleConfigIgnore =
        RuleConfig#{ignore => [fail_module_naming_1_convention_1]},
    [] = elvis_style:module_naming_convention(
            ElvisConfig, FileFail, RuleConfigIgnore
         ).


-spec verify_state_record_and_type(config()) -> any().
verify_state_record_and_type(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    PathPass = "pass_state_record_and_type.erl",
    {ok, FilePass} = elvis_test_utils:find_file(SrcDirs, PathPass),
    [] = elvis_style:state_record_and_type(ElvisConfig, FilePass, #{}),

    PathPassGenStateM = "pass_state_record_and_type_gen_statem.erl",
    {ok, FilePassGenStateM} = elvis_test_utils:find_file(SrcDirs, PathPassGenStateM),
    [] = elvis_style:state_record_and_type(ElvisConfig, FilePassGenStateM, #{}),

    PathFail = "fail_state_record_and_type.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),
    [_] = elvis_style:state_record_and_type(ElvisConfig, FileFail, #{}),

    PathFail1 = "fail_state_type.erl",
    {ok, FileFail1} = elvis_test_utils:find_file(SrcDirs, PathFail1),
    [_] = elvis_style:state_record_and_type(ElvisConfig, FileFail1, #{}),

    PathFailGenStateMType = "fail_state_record_and_type_gen_statem_type.erl",
    {ok, FileFailGenStateMType} = elvis_test_utils:find_file(SrcDirs, PathFailGenStateMType),
    [_] = elvis_style:state_record_and_type(ElvisConfig, FileFailGenStateMType, #{}),

    PathPassGenStateMState = "fail_state_record_and_type_gen_statem_state.erl",
    {ok, FilePassGenStateMState} = elvis_test_utils:find_file(SrcDirs, PathPassGenStateMState),
    [_] = elvis_style:state_record_and_type(ElvisConfig, FilePassGenStateMState, #{}).

-spec verify_no_spec_with_records(config()) -> any().
verify_no_spec_with_records(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    PathFail = "fail_no_spec_with_records.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),
    [_, _, _] = elvis_style:no_spec_with_records(ElvisConfig, FileFail, #{}),

    PathPass = "pass_no_spec_with_records.erl",
    {ok, FilePass} = elvis_test_utils:find_file(SrcDirs, PathPass),
    [] = elvis_style:no_spec_with_records(ElvisConfig, FilePass, #{}).

-spec verify_dont_repeat_yourself(config()) -> any().
verify_dont_repeat_yourself(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    PathFail = "fail_dont_repeat_yourself.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),
    RuleConfig5 = #{min_complexity => 5},
    Res1 = elvis_style:dont_repeat_yourself(ElvisConfig, FileFail, RuleConfig5),
    2 = length(Res1),

    RuleConfig9 = #{min_complexity => 9},
    Res2 = elvis_style:dont_repeat_yourself(ElvisConfig, FileFail, RuleConfig9),
    1 = length(Res2),

    IgnoreRule = #{ignore => [fail_dont_repeat_yourself]},
    [] = elvis_style:dont_repeat_yourself(ElvisConfig, FileFail, IgnoreRule),

    PathPass = "pass_dont_repeat_yourself.erl",
    {ok, FilePass} = elvis_test_utils:find_file(SrcDirs, PathPass),
    [] = elvis_style:dont_repeat_yourself(ElvisConfig, FilePass, RuleConfig5).

-spec verify_max_module_length(config()) -> any().
verify_max_module_length(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),
    PathFail = "fail_max_module_length.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),

    CountAllRuleConfig = #{count_comments => true, count_whitespace => true},

    ct:comment("Count whitespace and comment lines"),
    RuleConfig = CountAllRuleConfig#{max_length => 10},

    [_] = elvis_style:max_module_length(ElvisConfig, FileFail, RuleConfig),

    RuleConfig1 = CountAllRuleConfig#{max_length => 14},
    [_] = elvis_style:max_module_length(ElvisConfig, FileFail, RuleConfig1),

    RuleConfig2 = CountAllRuleConfig#{max_length => 15},
    [] = elvis_style:max_module_length(ElvisConfig, FileFail, RuleConfig2),

    ct:comment("Don't count whitespace lines"),
    WhitespaceRuleConfig = CountAllRuleConfig#{count_whitespace => false},

    RuleConfig3 = WhitespaceRuleConfig#{max_length => 3},
    [_] = elvis_style:max_module_length(ElvisConfig, FileFail, RuleConfig3),

    RuleConfig4 = WhitespaceRuleConfig#{max_length => 4},
    [_] = elvis_style:max_module_length(ElvisConfig, FileFail, RuleConfig4),

    RuleConfig5 = WhitespaceRuleConfig#{max_length => 5},
    [] = elvis_style:max_module_length(ElvisConfig, FileFail, RuleConfig5),

    ct:comment("Don't count comment or whitespace lines"),
    NoCountRuleConfig = WhitespaceRuleConfig#{count_comments => false},

    RuleConfig6 = NoCountRuleConfig#{max_length => 1},
    [_] = elvis_style:max_module_length(ElvisConfig, FileFail, RuleConfig6),

    RuleConfig7 = NoCountRuleConfig#{max_length => 2},
    [_] = elvis_style:max_module_length(ElvisConfig, FileFail, RuleConfig7),

    RuleConfig8 = NoCountRuleConfig#{max_length => 3},
    [] = elvis_style:max_module_length(ElvisConfig, FileFail, RuleConfig8),

    {comment, ""}.

-spec verify_max_function_length(config()) -> any().
verify_max_function_length(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    PathFail = "fail_max_function_length.erl",
    ModuleFail = fail_max_function_length,

    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),

    CountAllRuleConfig = #{count_comments => true, count_whitespace => true},

    ct:comment("Count whitespace and comment lines"),
    RuleConfig = CountAllRuleConfig#{max_length => 4},
    [_, _, _] =
        elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig),

    RuleConfig1 = CountAllRuleConfig#{max_length => 9},
    [_, _] =
        elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig1),

    RuleConfig2 = CountAllRuleConfig#{max_length => 14},
    [_] = elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig2),

    RuleConfig3 = CountAllRuleConfig#{max_length => 15},
    [] = elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig3),

    ct:comment("Don't count whitespace lines"),
    WhitespaceRuleConfig = CountAllRuleConfig#{count_whitespace => false},

    RuleConfig4 = WhitespaceRuleConfig#{max_length => 3},
    [_, _, _] =
        elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig4),

    RuleConfig5 = WhitespaceRuleConfig#{max_length => 7},
    [_, _] =
        elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig5),

    RuleConfig6 = WhitespaceRuleConfig#{max_length => 8},
    [_] = elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig6),

    RuleConfig7 = WhitespaceRuleConfig#{max_length => 11},
    [_] = elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig7),

    RuleConfig8 = WhitespaceRuleConfig#{max_length => 12},
    [] = elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig8),

    ct:comment("Don't count comment or whitespace lines"),
    NoCountRuleConfig = WhitespaceRuleConfig#{count_comments => false},

    RuleConfig9 = NoCountRuleConfig#{max_length => 1},
    [_, _, _] =
        elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig9),

    RuleConfig10 = NoCountRuleConfig#{max_length => 2},
    [] = elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig10),

    IgnoredFunctions = [{ModuleFail, f15}, {ModuleFail, f10, 1}],
    RuleConfig11 = RuleConfig5#{ignore => IgnoredFunctions},
    [] = elvis_style:max_function_length(ElvisConfig, FileFail, RuleConfig11),

    {comment, ""}.

-spec verify_no_debug_call(config()) -> any().
verify_no_debug_call(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    PathFail = "fail_no_debug_call.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),

    [_, _, _, _, _, _] = elvis_style:no_debug_call(ElvisConfig, FileFail, #{}),

    RuleConfig = #{ignore => [fail_no_debug_call]},
    [] = elvis_style:no_debug_call(ElvisConfig, FileFail, RuleConfig),

    RuleConfig2 = #{debug_functions => [{ct, pal, 2}]},
    [_] = elvis_style:no_debug_call(ElvisConfig, FileFail, RuleConfig2),

    RuleConfig3 = #{debug_functions => [{ct, pal}]},
    [_, _] = elvis_style:no_debug_call(ElvisConfig, FileFail, RuleConfig3),

    RuleConfig4 = #{debug_functions => [{io, format}]},
    [_, _, _] =
        elvis_style:no_debug_call(ElvisConfig, FileFail, RuleConfig4),

    RuleConfig5 = #{debug_functions => [{ct, print}]},
    [_, _] = elvis_style:no_debug_call(ElvisConfig, FileFail, RuleConfig5).

%% We test no_call and no_common_caveats_call by building the equivalent config and make sure that
%% other than defaults, they behave the same
-spec verify_no_common_caveats_call(config()) -> any().
verify_no_common_caveats_call(_Config) ->
    verify_no_call_flavours(no_common_caveats_call, fun elvis_style:no_common_caveats_call/3, caveat_functions, 6).

-spec verify_no_call(config()) -> any().
verify_no_call(_Config) ->
    verify_no_call_flavours(no_call, fun elvis_style:no_call/3, no_call_functions, 0).

-spec verify_no_call_flavours(atom(), fun(), atom(), non_neg_integer()) -> any().
verify_no_call_flavours(RuleName, RuleFun, RuleConfigMapKey, ExpectedDefaultRuleMatchCount) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    PathFail = "fail_no_call_classes.erl",
    {ok, FileFail} = elvis_test_utils:find_file(SrcDirs, PathFail),

    assert_length(ExpectedDefaultRuleMatchCount, RuleFun(ElvisConfig, FileFail, #{}), RuleName),

    RuleConfig = #{ignore => [fail_no_call_classes]},
    assert_length(0, RuleFun(ElvisConfig, FileFail, RuleConfig), RuleName),

    RuleMatchTuples = [{{timer, send_after, 2}, 1},
                       {{timer, send_after, 3}, 1},
                       {{timer, send_interval, 2}, 1},
                       {{timer, send_interval, 3}, 1},
                       {{erlang, size, 1}, 2},
                       {{timer, send_after}, 2}
                      ],

    lists:foreach(fun({FunSpec, ExpectedCount}) ->
                      ThisRuleConfig = maps:from_list([{RuleConfigMapKey, [FunSpec]}]),
                      Result = RuleFun(ElvisConfig, FileFail, ThisRuleConfig),
                      assert_length(ExpectedCount, Result, RuleName)
                  end,
                  RuleMatchTuples).

-spec verify_no_nested_try_catch(config()) -> any().
verify_no_nested_try_catch(_Config) ->
    ElvisConfig = elvis_test_utils:config(),

    SrcDirs = elvis_config:dirs(ElvisConfig),

    Module = fail_no_nested_try_catch,
    Path   = atom_to_list(Module) ++ ".erl",
    {ok, File} = elvis_test_utils:find_file(SrcDirs, Path),
    [
     #{line_num := 18},
     #{line_num := 33},
     #{line_num := 40}
    ] = elvis_style:no_nested_try_catch(ElvisConfig, File, #{}),

    [] = elvis_style:no_nested_try_catch(ElvisConfig, File,
                                         #{ignore => [Module]}).

-spec verify_atom_naming_convention(config()) -> any().
verify_atom_naming_convention(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    BaseRegex = "^([a-z][a-z0-9_]+)$",

    % pass

    PassModule = pass_atom_naming_convention,
    PassPath = atom_to_list(PassModule) ++ ".erl",
    {ok, PassFile} = elvis_test_utils:find_file(SrcDirs, PassPath),

    [] = elvis_style:atom_naming_convention(ElvisConfig, PassFile, #{ regex => BaseRegex }),

    % fail

    FailModule = fail_atom_naming_convention,
    FailPath = atom_to_list(FailModule) ++ ".erl",
    {ok, FailFile} = elvis_test_utils:find_file(SrcDirs, FailPath),

    [_,_,_,_,_,_,_,_,_,_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => BaseRegex,
                                                                       enclosed_atoms => same }),
    [_,_,_,_,_,_,_,_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => "^([a-zA-Z_]+)$",
                                                                       enclosed_atoms => same }),
    [_,_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => "^([a-zA-Z_' \\\\]+)$",
                                                                       enclosed_atoms => same }),
    [_,_,_,_,_,_,_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => "^([a-zA-Z\-_]+)$",
                                                                       enclosed_atoms => same }),
    [_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => "^([a-zA-Z\-_' \\\\]+)$",
                                                                       enclosed_atoms => same }),
    []
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => "^([0-9]?[a-zA-Z\-_]+)$" }),
    []
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => BaseRegex,
                                                                       ignore => [fail_atom_naming_convention] }),
    KeepRegex = "^([a-zA-Z0-9_]+)$",
    [_,_,_,_,_,_,_,_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => KeepRegex,
                                                                       enclosed_atoms => "^([a-z][a-z0-9A-Z_]*)$" }),
    [_,_,_,_,_,_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => KeepRegex,
                                                                       enclosed_atoms => "^([a-z][a-z0-9A-Z_' \\\\]*)$" }),
    [_,_,_,_,_,_,_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => KeepRegex,
                                                                       enclosed_atoms => "^([a-z][\-a-z0-9A-Z_]*)$" }),
    [_,_,_,_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => KeepRegex,
                                                                       enclosed_atoms => "^([0-9a-z][\-a-z0-9A-Z_' \\\\]*)$" }),
    [_,_,_,_,_]
        = elvis_style:atom_naming_convention(ElvisConfig, FailFile, #{ regex => KeepRegex,
                                                                       enclosed_atoms => "^([\\\\][\-a-z0-9A-Z_' \\\\]*)$" }).

-spec results_are_ordered_by_line(config()) -> true.
results_are_ordered_by_line(_Config) ->
    ElvisConfig = elvis_test_utils:config(),
    {fail, Results} = elvis_core:rock(ElvisConfig),
    true = lists:all(fun(X) -> X end, is_item_line_sort(Results)).

-spec verify_elvis_attr_atom_naming_convention(config()) -> true.
verify_elvis_attr_atom_naming_convention(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_atom_naming_convention_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_dont_repeat_yourself(config()) -> true.
verify_elvis_attr_dont_repeat_yourself(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_dont_repeat_yourself_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_function_naming_convention(config()) -> true.
verify_elvis_attr_function_naming_convention(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_function_naming_convention_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_god_modules(config()) -> true.
verify_elvis_attr_god_modules(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_god_modules_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_invalid_dynamic_call(config()) -> true.
verify_elvis_attr_invalid_dynamic_call(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_invalid_dynamic_call_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_line_length(config()) -> true.
verify_elvis_attr_line_length(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_line_length_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_macro_module_names(config()) -> true.
verify_elvis_attr_macro_module_names(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_macro_module_names_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_macro_names(config()) -> true.
verify_elvis_attr_macro_names(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_macro_names_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_max_function_length(config()) -> true.
verify_elvis_attr_max_function_length(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_max_function_length_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_max_module_length(config()) -> true.
verify_elvis_attr_max_module_length(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_max_module_length_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_module_naming_convention(config()) -> true.
verify_elvis_attr_module_naming_convention(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_module_naming-convention_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_nesting_level(config()) -> true.
verify_elvis_attr_nesting_level(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_nesting_level_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_no_behavior_info(config()) -> true.
verify_elvis_attr_no_behavior_info(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_no_behavior_info_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_no_call(config()) -> true.
verify_elvis_attr_no_call(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_no_call_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_no_debug_call(config()) -> true.
verify_elvis_attr_no_debug_call(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_no_debug_call_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_no_if_expression(config()) -> true.
verify_elvis_attr_no_if_expression(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),
    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_no_if_expression_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_no_nested_try_catch(config()) -> true.
verify_elvis_attr_no_nested_try_catch(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_no_nested_try_catch_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_no_spaces(config()) -> true.
verify_elvis_attr_no_spaces(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_no_spaces_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_no_spec_with_records(config()) -> true.
verify_elvis_attr_no_spec_with_records(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_no_spec_with_records_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_no_tabs(config()) -> true.
verify_elvis_attr_no_tabs(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_no_tabs_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_no_trailing_whitespace(config()) -> true.
verify_elvis_attr_no_trailing_whitespace(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_no_trailing_whitespace_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_operator_spaces(config()) -> true.
verify_elvis_attr_operator_spaces(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_operator_spaces_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_state_record_and_type(config()) -> true.
verify_elvis_attr_state_record_and_type(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_state_record_and_type_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_used_ignored_variable(config()) -> true.
verify_elvis_attr_used_ignored_variable(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_used_ignored_variable_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

-spec verify_elvis_attr_variable_naming_convention(config()) -> true.
verify_elvis_attr_variable_naming_convention(_Config) ->
    ElvisConfig = elvis_test_utils:config(erl_files),
    SrcDirs = elvis_config:dirs(ElvisConfig),

    {ok, File} = elvis_test_utils:find_file(SrcDirs, "pass_variable_naming_convention_elvis_attr.erl"),
    verify_elvis_attr_do_rock_results(elvis_core:do_rock(File, ElvisConfig)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

verify_elvis_attr_do_rock_results({ok, #{ rules := RuleResults }}) ->
    [[] = Items || #{ items := Items } <- RuleResults],
    true.

-spec is_item_line_sort([elvis_result:file()]) -> [boolean()].
is_item_line_sort(Result) ->
    Items = [Items
             || #{rules := Rules} <- Result,
                #{items := Items} <- Rules],
    lists:map(fun is_list_sort/1, Items).

-spec is_list_sort([any()]) -> boolean().
is_list_sort([_]) -> true;
is_list_sort([]) -> true;
is_list_sort([#{line_num := Line1} | T1]) ->
    [#{line_num := Line2} | _] = T1,
    case Line1 =< Line2 of
        true -> is_list_sort(T1);
        false -> false
    end.

-spec assert_length(non_neg_integer(), [any()], atom()) -> any().
assert_length(Expected, List, RuleName) ->
    case length(List) of
        Expected -> ok;
        _ -> throw({unexpected_response_length, RuleName, {expected, Expected}, {got, List}})
    end.
