%%%-------------------------------------------------------------------
%%% @doc
%%% Ecompasses all calculations related to Counter Data Type
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_rrd_counter).
-export([step/2]).

step(RRD, _Values) ->
    io:format("~p just stepped~n", [?MODULE]),
    RRD.
