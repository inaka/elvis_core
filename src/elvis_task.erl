-module(elvis_task).

-export([chunk_fold/6]).

%% @doc chunk_fold evaluates apply(Module, Function, [Elem|ExtrArgs]) for
%% every element Elem in JobItemList in parallel with max concurrcy factor
%% equal to Concurrency. On successful evaluation FunAcc function is called
%% with the result of successful execution as a first parameter and accumulator
%% as a second parameter.
-spec chunk_fold(
    FunWork :: {Module :: module(), Function :: atom()},
    FunAcc :: fun((NewElem :: term(), Acc :: term()) -> Acc :: term()),
    InitialAcc :: term(),
    ExtraArgs :: list(),
    JoinItemList :: list(),
    Concurrency :: non_neg_integer()
) ->
    {ok, FinalAcc :: term()} | {error, term()}.
chunk_fold({M, F} = FunWork, FunAcc, InitialAcc, ExtraArgs, List, ChunkSize) when
    is_atom(M),
    is_atom(F),
    is_function(FunAcc, 2),
    is_list(ExtraArgs),
    is_list(List),
    is_integer(ChunkSize) andalso (ChunkSize > 0)
->
    try
        Term =
            do_in_parallel(
                FunWork,
                FunAcc,
                ExtraArgs,
                List,
                _MaxW = ChunkSize,
                _RemainW = ChunkSize,
                InitialAcc,
                []
            ),
        {ok, Term}
    catch
        {T, E} ->
            {error, {T, E}}
    end.

do_in_parallel(_FunWork, FunAcc, _ExtraArgs, [], _MaxW, _RemainW, AccR, AccG) ->
    gather_all_results(FunAcc, AccR, AccG);
do_in_parallel(FunWork, FunAcc, ExtraArgs, List, MaxW, 0, AccR, AccG) ->
    {AccR1, AccG1, N} = gather_results(FunAcc, AccR, AccG),
    do_in_parallel(FunWork, FunAcc, ExtraArgs, List, MaxW, erlang:min(N, MaxW), AccR1, AccG1);
do_in_parallel(FunWork, FunAcc, ExtraArgs, List, MaxW, RemainW, AccR, AccG) ->
    {WorkToBeDone, WorkRemain} =
        try lists:split(RemainW, List) of
            Res ->
                Res
        catch
            error:badarg ->
                {List, []}
        end,

    WrkRefs = [start_worker(FunWork, ExtraArgs, WorkPiece) || WorkPiece <- WorkToBeDone],
    do_in_parallel(FunWork, FunAcc, ExtraArgs, WorkRemain, MaxW, 0, AccR, WrkRefs ++ AccG).

start_worker(FunWork, ExtraArgs, Arg) ->
    Parent = self(),
    Key = spawn_monitor(fun() -> do_work(Parent, FunWork, ExtraArgs, Arg) end),
    Key.

-spec do_work(pid(), {module(), atom()}, list(), term()) -> no_return().
do_work(Parent, {M, F}, ExtraArgs, Arg) ->
    try erlang:apply(M, F, [Arg | ExtraArgs]) of
        {ok, Results} ->
            exit({Parent, {ok, Results}});
        Unexpected ->
            Error = {error, {badreturn, Unexpected}},
            exit({Parent, {error, Error}})
    catch
        T:E ->
            exit({Parent, {error, {T, E}}})
    end.

gather_all_results(AccF, AccR, Remain) ->
    {AccR1, _, _} = gather_results0(AccF, AccR, Remain, 0, infinity),
    AccR1.

gather_results(AccF, AccR, AccG) ->
    {AccG1, Res} = gather(infinity, AccG),
    AccR1 = accumulate(AccF, AccR, Res, AccG1),
    gather_results0(AccF, AccR1, AccG1, 1, 0).

gather_results0(_AccF, AccR, [], N, _Timeout) ->
    {AccR, [], N};
gather_results0(AccF, AccR, AccG, N, Timeout) ->
    case gather(Timeout, AccG) of
        timeout ->
            {AccR, AccG, N};
        {AccG1, Res} ->
            AccR1 = accumulate(AccF, AccR, Res, AccG1),
            gather_results0(AccF, AccR1, AccG1, N + 1, Timeout)
    end.

accumulate(AccF, AccR, Res, AccG) ->
    try
        {ok, AccR1} = AccF(Res, AccR),
        AccR1
    catch
        T:E ->
            _ = cleanup(AccG),
            throw({T, E})
    end.

cleanup(AccG) ->
    [demonitor_and_exit(MRef, Pid) || {Pid, MRef} <- AccG].

demonitor_and_exit(MRef, Pid) ->
    erlang:demonitor(MRef, [flush]),
    erlang:exit(Pid, kill).

gather(Timeout, AccG) ->
    Self = self(),
    receive
        {'DOWN', _MonRef, process, Pid, {Self, Res}} ->
            AccG1 = lists:keydelete(Pid, 1, AccG),
            case Res of
                {ok, Res0} ->
                    {AccG1, Res0};
                {error, {T, E}} ->
                    _ = cleanup(AccG1),
                    throw({T, E})
            end
    after Timeout ->
        timeout
    end.
