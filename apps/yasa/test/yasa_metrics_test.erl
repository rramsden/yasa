-module(yasa_metrics_test).
-include_lib("eunit/include/eunit.hrl").

gauge_test() ->
    RRD0 = yasa_rrd:new([{1,2}]),
    RRD1 = yasa_metrics_gauge:insert([{120, 1.0}], RRD0),
    ?assertMatch([[{120, 1.0}]], yasa_rrd:points(RRD1)).

counter_test() ->
    RRD0 = yasa_rrd:new([{1,2}]),
    RRD1 = yasa_metrics_counter:insert([{120, 5}], RRD0),
    ?debugHere,
    ?assertMatch([[{120, 5}]], yasa_rrd:points(RRD1)),

    RRD2 = yasa_metrics_counter:insert([{121, -2}], RRD1),
    ?assertMatch([[{121, 3},{120, 5}]], yasa_rrd:points(RRD2)).
