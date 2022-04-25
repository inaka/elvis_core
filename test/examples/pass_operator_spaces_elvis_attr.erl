%% Initial comment
%% Another initial comment
-module(pass_operator_spaces_elvis_attr).

-dialyzer({nowarn_function , function7/0}).

-elvis([{elvis_style , operator_spaces , #{rules => [{right , "++"} ,
                                                     {left , ","}]}}]).
-elvis([{elvis_style , no_space , #{rules => [{right , "("} , {left , ")"}]}}]).
-export([ function1/2
        , function2/2
        , function3/2
        , function4/2
        , function5/0
        , function6/0
        , function7/0
        , tag_filters/2
        , windows_newlines/0
        , unicode_characters/0
        ]).

%% No space before and after coma,on a comment.

function1(Should ,Fail) ->
    [Should ,Fail].

function2(Shouldnt , Fail) ->
    _Unless = [we , consider]++ [operands , as , well] ,
    _WithDash = Shouldnt - Fail.

function3(Shouldnt , Fail) ->
    {
      Shouldnt ++ "Hello ,Dont't Fail" ++ Fail ,
      'hello,don\'t fail' ,
      <<"hello,don't fail">>
    }.

function4(Should , <<_:10/binary , "," , _/binary>>) ->
    Should = [$, , "where $, represents the character ,"].

function5() ->
    User = #{name => <<"Juan">> , email => <<"juan@inaka.com">>} ,
    <<"juan@inaka.com">> = maps:get(email ,User).

function6() ->
    _MissingLeftSpace = 2+ 3 ,
    _MissingRightSpace = 2 +3 ,
    _Successful = 2 + 3 ,
    _AlsoSuccessful = +1.

function7() ->
    % commas within strings must be ignored
    Name = "anyone" ,
    re:run(Name , "^.{1,20}$" , [unicode]) ,
    RegExp = "^.{1,20}$" ,
    re:run(Name , RegExp , [unicode]).

tag_filters(DocName , #{conn := Conn} = State) ->
  TableName = atom_to_list(DocName) ,
  Sql = ["SELECT "
         " 'tag' AS \"type\", "
         " tag_name AS value, "
         " COUNT(1) AS \"count\" "
         "FROM ( "
         " SELECT unnest(regexp_split_to_array(tags, ',')) AS tag_name"
         " FROM " , TableName , " "
         ") AS tags "
         "GROUP BY tag_name "
         "ORDER BY tag_name "] ,
  Values = [] ,
  case {Conn , Sql , Values} of
    {ok , Maps , _} ->
      {ok , {raw , Maps} , State};
    {error , Error , _} ->
      {error , Error , State}
  end.

unicode_characters() ->
  <<"©"/utf8>> = <<"\\u00A9">> ,
  <<"ß"/utf8>> = <<"\\o337">> ,
  ok.

windows_newlines() ->
    <<_/bytes>> = <<"Foo" ,
                    "bar">> ,
    ok.
