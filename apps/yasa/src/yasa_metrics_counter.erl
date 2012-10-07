%%%-------------------------------------------------------------------
%%% @doc
%%% Ecompasses all calculations related to Counter Data Type
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_metrics_counter).
-export([insert/2]).

insert([], RRD) -> RRD;
insert([{Time, Value} | Rest], RRD0) ->
    FirstTable = hd(yasa_rrd:points(RRD0)),
    {_, RefVal} = case FirstTable of
        [] ->
            {nil, 0}; % first insert
        List ->
            hd(List) % grab last inserted
    end,
    RRD1 = yasa_rrd:add({Time, RefVal + Value}, RRD0),
    insert(Rest, RRD1).
