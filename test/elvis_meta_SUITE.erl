-module(elvis_meta_SUITE).
-author('euen@inaka.net').

-export([all/0]).
-export([dialyzer/1, xref/1]).

-type config() :: [{atom(), term()}].

-spec all() -> [dialyzer | xref].
all() -> [dialyzer, xref].

-spec dialyzer(config()) -> {comment, []}.
dialyzer(_Config) ->
  BaseDir = code:lib_dir(elvis_core),
  DefaultRebar3PltLoc = filename:join(BaseDir, "../../../default"),
  Plts = filelib:wildcard(filename:join(DefaultRebar3PltLoc, "*_plt")),
  Dirs = [filename:join(BaseDir, Dir) || Dir <- ["ebin", "test"]],
  Warnings = [error_handling, no_return, unmatched_returns],
  ct:comment("Dialyzer must emit no warnings"),
  Opts =
    [ {analysis_type, succ_typings}
    , {plts,          Plts}
    , {files_rec,     Dirs}
    , {check_plt,     true}
    , {warnings,      Warnings}
    ],
  [] = [dialyzer:format_warning(W, basename) || W <- dialyzer:run(Opts)],
  {comment, ""}.

-spec xref(config()) -> {comment, []}.
xref(_Config) ->
  BaseDir = code:lib_dir(elvis_core),
  Dirs = [filename:join(BaseDir, Dir) || Dir <- ["ebin", "test"]],
  XrefConfig = #{ dirs => Dirs
                , xref_defaults =>
                  [ {verbose, true}
                  , {recurse, true}
                  , {builtins, true}
                  ]
                },
  Checks = [ undefined_function_calls
           , locals_not_used
           , deprecated_function_calls
           ],
  ct:comment("There are no Warnings"),
  [] =
    [ Warning
    || Check <- Checks, Warning <- xref_runner:check(Check, XrefConfig)],
  {comment, ""}.
