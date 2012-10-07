-module(yasa_rrd).

-export([
    new/1,
    new/2,
    add/2,
    points/1,
    table/2,
    step_size/1,
    retentions/1
]).

-record(rrd, {
    archives = [],
    consolidation = average,
    xff
}).

-record(archive, {
    step,
    size,
    points = [] :: {TS :: number(), Value :: number()}
}).

-define(pval(K, List, Default), proplists:get_value(K, List, Default)).
-define(pval(K, List), proplists:get_value(K, List)).
-define(DAYS_FROM_GREGORIAN_BASE_TO_EPOCH, (1970*365+478)).
-define(SECONDS_FROM_GREGORIAN_BASE_TO_EPOCH,
         (?DAYS_FROM_GREGORIAN_BASE_TO_EPOCH * 24*60*60)).

new(Args) ->
    new(Args, []).

new(Args, Opts) ->
    Xff = ?pval(xff, Opts, 0.5),
    Consolidation = ?pval(consolidation, Opts, average),
    Divisible = divisibility_check(Args),

    case Divisible of
        true ->
            Archives = lists:map(fun({Step, Size}) -> #archive{step=Step, size=Size} end, Args),
            #rrd{archives=Archives, consolidation=Consolidation, xff=Xff};
        false ->
            {error, not_divisible}
    end.

points(RRD) ->
    lists:map(fun(Archive) -> Archive#archive.points end, RRD#rrd.archives).

retentions(Q) ->
    Archives = Q#rrd.archives,
    lists:map(fun(#archive{step=Step, size=Size}) -> {Step,Size} end, Archives).

step_size(Table) ->
    Table#archive.step.

table(N, Q) ->
    X = lists:nth(N, Q#rrd.archives),
    X#archive.points.

%% @doc
%% Neighbour values are the points we need to aggregate
%% into the next table up
%% @end
neighbours(T1, Higher, Lower) ->
    T2 = T1 - (T1 rem Higher#archive.step),
    {_, B} = lists:splitwith(fun({N, _}) -> N =/= T2 end, Higher#archive.points),
    Take = (T1 rem Lower#archive.step) + 1,
    lists:sublist(B, Take).

add({Time, _} = Point, #rrd{archives=Archives0} = RRD0) ->
    Diff = 0,
    case lists:splitwith(fun(A) -> Diff > A#archive.step end, Archives0) of
        {_, []} ->
            RRD0; % time is too old, ignore it
        {Left, [Highest0 | Rest]} ->
            InsertAt = Time - (Time rem Highest0#archive.step),
            Highest1 = insert(Point, InsertAt, Highest0),
            Propagate = propagate(Point, [Highest1 | Rest], RRD0#rrd.xff, RRD0#rrd.consolidation, []),
            RRD0#rrd{archives=(Left ++ [Highest1] ++ Propagate)}
    end.

propagate(_, [_], _, _, Acc) -> lists:reverse(Acc);
propagate({Time, _}, [Higher, Lower | Rest], XFF, Cons, Acc) ->
    % select data-points to aggregate into next table up
    Neighbours = neighbours(Time, Higher, Lower),
    CanAggregate = ((length(Neighbours) * Higher#archive.step) / Lower#archive.step) >= XFF,

    case CanAggregate of
        true ->
            NewPoint = {T2, V} = consolidate(Cons, Neighbours),
            LowerIndex = T2 - (T2 rem Lower#archive.step),
            Lower1 = insert({LowerIndex, V}, LowerIndex, Lower),
            propagate(NewPoint, [Lower1 | Rest], XFF, Cons, [Lower1 | Acc]);
        false ->
            lists:reverse([Lower | Acc])
    end.

insert(Point, InsertAt, #archive{size=Size, points=Points0} = Archive) ->
    Points1 = case proplists:get_value(InsertAt, Points0) of
        undefined -> [Point | lists:sublist(Points0, Size - 1)];
        _ -> lists:keyreplace(InsertAt, 1, Points0, Point)
    end,
    Archive#archive{points=Points1}.

consolidate(average, Points) ->
    {Times, Values} = lists:unzip(Points),
    Avg = lists:foldl(fun(N, Acc) -> N + Acc end, 0, Values) / length(Values),
    {hd(Times), Avg};
consolidate(min, Points) ->
    {Times, Values} = lists:unzip(Points),
    Min = lists:min(Values),
    {hd(Times), Min};
consolidate(max, Points) ->
    {Times, Values} = lists:unzip(Points),
    Max = lists:max(Values),
    {hd(Times), Max}.

now_secs() ->
    {Mega, Sec, _} = os:timestamp(),
    ?SECONDS_FROM_GREGORIAN_BASE_TO_EPOCH + Mega * 1000000 + Sec.

%%%
%% @private step sizes must be divisible

divisibility_check([{_, _}]) -> true;
divisibility_check([]) -> true;
divisibility_check([{Step1, _}, {Step2, _} = H2 | T]) ->
    case Step2 rem Step1 == 0 of
        true -> divisibility_check([H2 | T]);
        false -> false
    end.
