-module(elvis_result).

-compile({no_auto_import, [error/2]}).

%% API
-export([new_item/1, new_item/2, new_item/3, new/3, new/4, status/1, clean/1, print_results/1]).
-export([
    get_path/1,
    get_rules/1,
    get_name/1,
    get_items/1,
    get_message/1,
    get_info/1,
    get_line_num/1
]).

%% Types
-export_type([item/0, rule/0, file/0, elvis_error/0, elvis_warn/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Records
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type item() ::
    #{
        message => string(),
        info => iodata(),
        line_num => integer()
    }.
-type rule() ::
    #{
        scope => atom(),
        name => atom(),
        items => [item()]
    }.
-type file() :: #{file => string(), rules => [rule()]}.
-type elvis_error() :: #{error_msg => string(), info => list()}.
-type elvis_warn() :: #{warn_msg => string(), info => list()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% New

new_item(Format) ->
    new_item(Format, []).

new_item(Format, Data) ->
    new_item(Format, Data, #{line => -1}).

new_item(Format, Data, #{node := Node}) ->
    {Line, Column} = ktn_code:attr(location, Node),
    new_item(Format, Data, Node#{line => Line, columm => Column});
new_item(Format, Data, Attrs) ->
    Line = maps:get(line, Attrs, -1),
    Limit = maps:get(limit, Attrs, -1),
    Column = maps:get(column, Attrs, -1),
    new(item, Format, Data, {Line, Column}, Limit).

-spec new
    (item, string(), [term()]) -> item();
    (rule, {atom(), atom()}, [item()]) -> rule();
    (file, elvis_file:file(), [elvis_error() | rule()]) -> file();
    (error, string(), string()) -> elvis_error();
    (warn, string(), string()) -> elvis_warn().
% new(item, ...) is kept for backward compatibility, but discouraged
new(item, Msg, Info) ->
    new_item(Msg, Info);
new(rule, {Scope, Name}, Results) ->
    #{
        scope => Scope,
        name => Name,
        items => Results
    };
new(file, #{path := Path}, Rules) ->
    #{file => Path, rules => Rules};
new(error, Msg, Info) ->
    #{error_msg => Msg, info => Info};
new(warn, Msg, Info) ->
    #{warn_msg => Msg, info => Info}.

-spec new(item, string(), [term()], integer()) -> item().
new(item, Msg, Info, LineNum) ->
    new_item(Msg, Info, #{line => LineNum}).

new(item, Msg0, Info, {Line, Column}, Limit) ->
    Msg1 =
        case Line > 0 of
            false ->
                Msg0;
            _ ->
                "At line " ++ integer_to_list(Line) ++ ", " ++ Msg0
        end,
    Msg2 =
        case Column > 0 of
            false ->
                Msg1;
            _ ->
                ", column " ++ integer_to_list(Column) ++ ", " ++ Msg1
        end,
    Msg =
        case Limit of
            -1 ->
                Msg2;
            _ ->
                Msg2 ++ " (limit: " ++ integer_to_list(Limit) ++ ")"
        end,
    #{
        message => Msg ++ ".",
        info => Info,
        line_num => Line
    }.

%% Getters

-spec get_path(file()) -> string().
get_path(#{file := File}) ->
    File.

-spec get_rules(file()) -> [rule()].
get_rules(#{rules := Rules}) ->
    Rules.

-spec get_name(rule()) -> atom().
get_name(#{name := Name}) ->
    Name.

-spec get_items(rule()) -> [item()].
get_items(#{items := Items}) ->
    Items;
get_items(_) ->
    [].

-spec get_message(item()) -> string().
get_message(#{message := Message}) ->
    Message.

-spec get_info(item()) -> string().
get_info(#{info := Info}) ->
    Info.

-spec get_line_num(item()) -> integer().
get_line_num(#{line_num := LineNum}) ->
    LineNum.

%% Print

-spec print_results(file() | [elvis_warn()]) -> ok.
print_results(Results) ->
    Format = elvis_config:from_application_or_config(output_format, colors),
    print(Format, Results).

-spec print(plain | colors | parsable, [file()] | file()) -> ok.
print(_, []) ->
    ok;
print(Format, [Result | Results]) ->
    print(Format, Result),
    print(Format, Results);
%% File
print(Format, #{file := Path, rules := Rules}) ->
    case Format of
        parsable ->
            ok;
        _ ->
            case status(Rules) of
                ok ->
                    elvis_utils:notice("# ~s [{{green-bold}}OK{{white-bold}}]", [Path]);
                fail ->
                    elvis_utils:error("# ~s [{{red-bold}}FAIL{{white-bold}}]", [Path])
            end
    end,
    print_rules(Format, Path, Rules);
print(_, Error) ->
    print_error(Error).

print_rules(_Format, _File, []) ->
    ok;
print_rules(Format, File, [#{items := []} | Items]) ->
    print_rules(Format, File, Items);
print_rules(
    Format,
    File,
    [
        #{
            scope := Scope,
            items := Items,
            name := Name
        }
        | EItems
    ]
) ->
    case Format of
        parsable ->
            ok;
        _ ->
            elvis_utils:error(
                "  - ~p "
                "(https://github.com/inaka/elvis_core/tree/main/doc_rules/~p/~p.md)",
                [Name, Scope, Name]
            )
    end,
    print_item(Format, File, Name, Items),
    print_rules(Format, File, EItems);
print_rules(Format, File, [Error | Items]) ->
    print_error(Error),
    print_rules(Format, File, Items).

%% Item

print_item(
    Format,
    File,
    Name,
    [
        #{
            message := Msg,
            line_num := Ln,
            info := Info
        }
        | Items
    ]
) ->
    case Format of
        parsable ->
            FMsg = io_lib:format(Msg, Info),
            io:format("~s:~p:~p:~s~n", [File, Ln, Name, FMsg]);
        _ ->
            elvis_utils:error("    - " ++ Msg, Info)
    end,
    print_item(Format, File, Name, Items);
print_item(Format, File, Name, [Error | Items]) ->
    print_error(Error),
    print_item(Format, File, Name, Items);
print_item(_Format, _File, _Name, []) ->
    ok.

print_error(#{error_msg := Msg, info := Info}) ->
    elvis_utils:error_prn(Msg, Info);
print_error(#{warn_msg := Msg, info := Info}) ->
    elvis_utils:warn_prn(Msg, Info).

-spec status([file() | rule()]) -> ok | fail.
status([]) ->
    ok;
status([#{rules := Rules} | Files]) ->
    case status(Rules) of
        fail ->
            fail;
        ok ->
            status(Files)
    end;
status([#{items := []} | Rules]) ->
    status(Rules);
status(_Rules) ->
    fail.

%% @doc Removes files that don't have any failures.
-spec clean([file() | rule()]) -> [file() | rule()].
clean(Files) ->
    clean(Files, []).

-spec clean([file() | rule()], [file() | rule()]) -> [file() | rule()].
clean([], Result) ->
    lists:reverse(Result);
clean([#{rules := []} | Files], Result) ->
    clean(Files, Result);
clean([File = #{rules := Rules} | Files], Result) ->
    CleanRules = clean(Rules),
    NewFile = File#{rules => CleanRules},
    clean(Files, [NewFile | Result]);
clean([#{items := []} | Rules], Result) ->
    clean(Rules, Result);
clean([Rule | Rules], Result) ->
    clean(Rules, [Rule | Result]).
