-define(YASA_FILE_VERSION, 1.0).

-record(rrd_table, {
    step,
    size,
    counter = 0,
    entries = []
}).

-record(rrd, {
    version = ?YASA_FILE_VERSION,
    key,
    type :: gauge | counter,
    last_timestamp,
    tables :: [#rrd_table{}]
}).
