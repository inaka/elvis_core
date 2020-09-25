-module(pass_variable_naming_convention_elvis_attr).

-export([bad_variables_name/3]).

-elvis([{elvis_style, variable_naming_convention,
        #{regex => "^[a-zA-Z@_]+$"}}]).

%% CamelCase must be used for variables. Don’t
%% separate words in variables with _.

%% Cf. https://github.com/inaka/erlang_guidelines#variable-names

bad_variables_name(Im@Home, My_Way, _Bad_Ignored_Variable) ->
    Im@Home ++ My_Way.
