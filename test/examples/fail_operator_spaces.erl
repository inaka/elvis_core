%% Initial comment
%% Another initial comment
-module(fail_operator_spaces).

-feature(maybe_expr, enable).

-dialyzer({nowarn_function, function7/0}).

-export([ function1/2
        , function2/2
        , function3/2
        , function4/2
        , function5/0
        , function6/0
        , function7/0
        , function8/0
        , function9/0
        , tag_filters/2
        , unicode_characters/0
        , windows_newlines/0
        , this/0
        , this/1
        , pass_more_operators/0
        , fail_more_operators/0
        , fail_no_space_excl/0
        , fail_maybe/0
        ]).

%% No space before and after coma,on a comment.

function1(Should,Fail) ->
    [Should,Fail].

function2(Shouldnt, Fail) ->
    _Unless = [we, consider]++ [operands, as, well],
    _WithDash = Shouldnt - Fail.

function3(Shouldnt, Fail) ->
    {
      Shouldnt ++ "Hello,Dont't Fail" ++ Fail,
      'hello,don\'t fail',
      <<"hello,don't fail">>
    }.

function4(Should, <<_:10/binary, ",", _/binary>>) ->
    Should = [$,, "where $, represents the character ,"].

function5() ->
    User = #{name => <<"Juan">>, email => <<"juan@inaka.com">>},
    <<"juan@inaka.com">> = maps:get(email,User).

function6() ->
    _MissingLeftSpace = 2+ 3,
    _MissingRightSpace = 2 +3,
    _Successful = 2 + 3,
    _AlsoSuccessful = +1.

function7() ->
    % commas within strings must be ignored
    Name = "anyone",
    re:run(Name, "^.{1,20}$", [unicode]),
    RegExp = "^.{1,20}$",
    re:run(Name, RegExp, [unicode]).

function8() ->
    [should| [fail]] ++ [should |[fail]] ++ [shouldnot | [fail]].

function9() ->
    [X|| X <- [fail]] ++ [X ||X <- [fail]] ++ [X || X <- [notfail]].

tag_filters(DocName, #{conn := Conn} = State) when is_list(DocName);is_binary(DocName) ->
  TableName = atom_to_list(DocName),
  Sql = ["SELECT "
         " 'tag' AS \"type\", "
         " tag_name AS value, "
         " COUNT(1) AS \"count\" "
         "FROM ( "
         " SELECT unnest(regexp_split_to_array(tags, ',')) AS tag_name"
         " FROM ", TableName, " "
         ") AS tags "
         "GROUP BY tag_name "
         "ORDER BY tag_name "],
  Values = [],
  case {Conn, Sql, Values} of
    {ok, Maps, _} when Maps=:=#{};Conn=:=established ->
      {ok, {raw, Maps}, State};
    {error, Error, _} ->
      {error, Error, State};{finally, this, _} -> nok
  end.

unicode_characters() ->
  <<"©"/utf8>> = <<"\\u00A9">>,
  <<"ß"/utf8>> = <<"\\o337">>,
  ok.

windows_newlines() ->
    <<_/bytes>> = <<"Foo",
                    "bar">>,
    ok.

-spec this()
-> should_not_crash.
this()
-> should_not_crash.

-spec this(shouldnt_either)
-> -1.
this(shouldnt_either)
-> A = 1
- 2, A.


-spec pass_more_operators() -> R when R :: tuple().
pass_more_operators() ->
    D = "Elvis should not complain this function "
        "since operators are properly spaced out: "
        "=,+,-,*,/,=<,<,>,>=,==,=:=,/=,=/=,--,=>,:=,<-,<=,||,|,::,->",
    X = 1 + 2 - 3 * 4 / 5,
    M = #{d => D, x => X},
    {
        X =< 1,
        X < 2,
        X > 3,
        X >= 4,
        X == 5,
        X =:= 6.0,
        X /= 7,
        X =/= 8,
        D -- "",
        M#{d => D, x := X},
        [A || A <- D],
        << <<A>> || <<A>> <= list_to_binary(D) >>,
        [X | D]
    }.

-spec fail_more_operators()->R when R::tuple().
fail_more_operators()->
    D="Elvis should complain this function "
      "since operators have no space around them.",
    X=1+2-3*4/5,
    M=#{d=>D, x=>X},
    {
        X=<1,
        X<2,
        X>3,
        X>=4,
        X==5,
        X=:=6.0,
        X/=7,
        X=/=8,
        D--"",
        M#{d=>D, x:=X},
        [A||A<- D],
        <<<<$>>>||<<$>>><=<<$>>>>>,
        [X|D]
    }.

fail_no_space_excl() ->
    self()!'a'.

fail_maybe() ->
    maybe
        A = 2,
        A?=throw(error)
    else
        _ ->
            ok
    end.
