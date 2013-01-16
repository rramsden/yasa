-module(yasa_rrd_tests).
-include_lib("eunit/include/eunit.hrl").

-define(EPOCH, 120).

retentions_test() ->
    Q0 = yasa_rrd:new([{1,2}, {2,2}, {4,2}]),
    ?assertEqual([{1,2}, {2,2}, {4,2}], yasa_rrd:retentions(Q0)).

raises_errors_test() ->
    {error, not_divisible} = yasa_rrd:new([{4,1}, {2,1}]),
    {error, not_divisible} = yasa_rrd:new([{2,1}, {3,1}]).

consolidation_test() ->
    A0 = yasa_rrd:new([{1,2}, {2,2}, {4,2}], [{consolidation, min}]),
    A1 = yasa_rrd:add({?EPOCH, 1.0}, A0),
    A2 = yasa_rrd:add({?EPOCH + 1, 2.0}, A1),

    ?assertEqual([{121, 2.0}, {120, 1.0}], yasa_rrd:table(1, A2)),
    ?assertEqual([{120, 1.0}], yasa_rrd:table(2, A2)),
    ?assertEqual([{120, 1.0}], yasa_rrd:table(3, A2)),

    B0 = yasa_rrd:new([{1,2}, {2,2}, {4,2}], [{consolidation, max}]),
    B1 = yasa_rrd:add({?EPOCH, 1.0}, B0),
    B2 = yasa_rrd:add({?EPOCH + 1, 2.0}, B1),

    ?assertEqual([{121, 2.0}, {120, 1.0}], yasa_rrd:table(1, B2)),
    ?assertEqual([{120, 2.0}], yasa_rrd:table(2, B2)),
    ?assertEqual([{120, 2.0}], yasa_rrd:table(3, B2)),

    C0 = yasa_rrd:new([{1,2}, {2,2}, {4,2}], [{consolidation, average}]),
    C1 = yasa_rrd:add({?EPOCH, 1.0}, C0),
    C2 = yasa_rrd:add({?EPOCH + 1, 2.0}, C1),

    ?assertEqual([{121, 2.0}, {120, 1.0}], yasa_rrd:table(1, C2)),
    ?assertEqual([{120, 1.5}], yasa_rrd:table(2, C2)),
    ?assertEqual([{120, 1.5}], yasa_rrd:table(3, C2)).

aggregation_test() ->
    XFactor = 0.5,
    Q0 = yasa_rrd:new([{1,2}, {2,2}, {4,2}], [{xff, XFactor}]),
    Q1 = yasa_rrd:add({?EPOCH, 1.0}, Q0),

    ?assertEqual([{120, 1.0}], yasa_rrd:table(1, Q1)),
    ?assertEqual([{120, 1.0}], yasa_rrd:table(2, Q1)),
    ?assertEqual([{120, 1.0}], yasa_rrd:table(3, Q1)),

    Q2 = yasa_rrd:add({?EPOCH + 1, 2.0}, Q1),

    ?assertEqual([{121, 2.0}, {120, 1.0}], yasa_rrd:table(1, Q2)),
    ?assertEqual([{120, 1.5}], yasa_rrd:table(2, Q2)),
    ?assertEqual([{120, 1.5}], yasa_rrd:table(3, Q2)),

    Q3 = yasa_rrd:add({?EPOCH + 2, 3.0}, Q2),
    Q4 = yasa_rrd:add({?EPOCH + 3, 4.0}, Q3),

    ?assertEqual([{123, 4.0}, {122, 3.0}], yasa_rrd:table(1, Q4)),
    ?assertEqual([{122, 3.5}, {120, 1.5}], yasa_rrd:table(2, Q4)),
    ?assertEqual([{120, 2.5}], yasa_rrd:table(3, Q4)),

    Q5 = yasa_rrd:add({?EPOCH + 4, 5.0}, Q4),
    Q6 = yasa_rrd:add({?EPOCH + 5, 6.0}, Q5),
    Q7 = yasa_rrd:add({?EPOCH + 6, 7.0}, Q6),
    Q8 = yasa_rrd:add({?EPOCH + 7, 8.0}, Q7),

    ?assertEqual([{127, 8.0}, {126, 7.0}], yasa_rrd:table(1, Q8)),
    ?assertEqual([{126, 7.5}, {124, 5.5}], yasa_rrd:table(2, Q8)),
    ?assertEqual([{124, 6.5}, {120, 2.5}], yasa_rrd:table(3, Q8)).
