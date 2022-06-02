-module(fail_no_nested_try_catch).

-ignore_xref({perhaps, throw, 1}).
-ignore_xref({a_function, that_deals, 2}).

-dialyzer({nowarn_function, bad2/0}).

-export([
         bad1/0,
         bad2/0,
         good1/0,
         good2/0
        ]).

bad1() ->
  try
    perhaps:throw(exception1),
    try
      perhaps:throw(exception2),
      "We are safe!"
    catch
      _:exception2 ->
        "Oh, no! Exception #2"
    end
  catch
    _:exception1 ->
      "Bummer! Exception #1"
  end.

bad2() ->
  try
    perhaps:throw(exception1),
    try
      perhaps:throw(exception2),
      "We are safe!"
    catch
      _:exception2 ->
        "Oh, no! Exception #2"
    end,
    try
      perhaps:throw(exception3),
      "We are safe!"
    catch
      _:exception3 ->
        "Oh, no! Exception #3"
    end
  catch
    _:exception1 ->
      "Bummer! Exception #1"
  end.

good1() ->
  try
    perhaps:throw(exception1),
    perhaps:throw(exception2),
    "We are safe!"
  catch
    _:exception1 ->
      "Bummer! Exception #1";
    _:exception2 ->
      "Oh, no! Exception #2"
  end.

good2() ->
  try
    perhaps:throw(exception1),
    a_function:that_deals(with, exception2),
    "We are safe!"
  catch
    _:exception1 ->
      "Bummer! Exception #1"
  end.
