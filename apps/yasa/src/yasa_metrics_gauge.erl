%%%-------------------------------------------------------------------
%%% @doc
%%% Ecompasses all calculations related to Gauge Data Type
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_rrd_gauge).
-export([step/2]).

step(RRD, [{TS, _}|_] = Values) ->
    N = consolidation(Values),
    yasa_rrd:push(RRD, {TS, N}).

consolidation(Values) ->
    lists:foldl(fun({_TS, Val}, Acc) -> Val + Acc end, 0, Values) / length(Values).
