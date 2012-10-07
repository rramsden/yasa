%%%-------------------------------------------------------------------
%%% @doc
%%% Ecompasses all calculations related to Gauge Data Type
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_metrics_gauge).
-export([insert/2]).

insert([], RRD) -> RRD;
insert([Point | Rest], RRD0) ->
    RRD1 = yasa_rrd:add(Point, RRD0),
    insert(Rest, RRD1).
