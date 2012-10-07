YASA
====

YASA is a high performance stat aggregation daemon which collects numeric
time-series data based on given keys.  Data is then aggregated and periodically
dumped to individual Round Robin Databases. Since Yasa is able to load recently
touched RRD archives into state its able to significantly reduce its I/O
footprint and quickly service real-time data without constantly reading/writing to disk.

Notes
=====

We submitted YASA for this years Spawnfest competition and it generated
a lot of interest. However, it was insanely buggy having been written in 48-hours
so right now I'm working out the kinks and aiming for a release in November.
Stay tuned!

Supported Metrics
=================

Counters
--------

Counter metrics provide increment and decrement capabilities for a single scalar value.

    /api/v1/counter?key=key.name&value=<value>
    /api/v1/counter?key=key.name&values=[[<timestamp>, <value>], ...]

Gauges
------

Gauges are point-in-time single value metrics.

    /api/v1/gauge?key=key.name&value=<value>
    /api/v1/gauge?key=key.name&values=[[<timestamp>, <value>], ...]

Retrieving Data
---------------

    /api/get?key=key.name&range=-<integer_value><hour|min|sec|day|month|year>

WebSocket API
=============

You can connect to Yasa via a Websocket by opening up `/wsapi`
The Websocket API allows you to register and listen for multiple keys.

	{method: "register", key: "keyname", range: "-<integer_value><hour|min|sec|day|month|year>"}

To unregister from a registered feed.

	{method: "unregister", key: "keyname"}

To increment a counter:

	{method: "incr", key: "keyname", value: <integer_value> }

To set a gauge:

	{method: "set", key: "keyname", value: <integer_value> }

Configuration
=============

Since RRD files are fixed in sized you must define how much historical data you want to keep lying around.
Yasa will default to 60 samples of 1 second data and 1000 samples of 1 minute data. You can define as
many retentions as you like. For example, in your app.config:

	[
		{yasa,[
      {port, 8080}
			{retentions, [{1,60},{60,1000}]}
		]}
    ].
