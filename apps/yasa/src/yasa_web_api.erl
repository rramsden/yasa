-module(yasa_web_api).

-export([handle_range/1, reply/3, pval/2]).

handle_range(String) when is_binary(String) ->
    Regex = "-(\\d+)(hour|min|sec|day|month|year)",
    [Size_, Period_] = case re:run(String, Regex) of
        nomatch ->
            {error, nomatch};
        Match ->
            parse_matches(String, Match)
    end,
    Size = list_to_integer(binary_to_list(Size_)),
    Period = list_to_atom(binary_to_list(Period_)),
    Reply = to_range(Size, Period),
    Reply.

reply(Key, <<"counter">>, Proplist) ->
    Value = pval(<<"value">>, Proplist),
    ok = yasa:counter(Key, [{timestamp(), bin_to_num(Value)}]),
    {200, <<"">>};
reply(Key, <<"gauge">>, Proplist) ->
    Value = pval(<<"value">>, Proplist),
    ok = yasa:gauge(Key, [{timestamp(), bin_to_num(Value)}]),
    {200, <<"">>};
reply(Key, <<"fetch.json">>, Proplist) ->
    [Start, End] = handle_range(pval(<<"range">>, Proplist)),
    Values = yasa:fetch(Key, Start, End),
    {200, lists:map(fun({T, V}) -> [T,V] end, Values)};
reply(undefined, <<"keys.json">>, _) ->
    {200, yasa:keys()};
reply(_, _, _) ->
    {500, <<"error:invalid request">>}.

pval(X, Req) when element(1, Req) == http_req ->
    {Val, _} = cowboy_http_req:qs_val(X, Req),
    Val;
pval(X, PL) ->
    proplists:get_value(X, PL).

% ============================================================================
% Internal Functions
% ============================================================================ 
parse_matches(String, {match, [_|T]}) ->
    parse_matches(String, T, []).
parse_matches(_, [], Acc) ->
    Acc;
parse_matches(String, [{Start, End} | Rest], Acc) ->
    <<_:Start/binary, Match:End/binary, _/binary>> = String,
    parse_matches(String, Rest, Acc ++ [Match]).

to_range(N, Period) ->
    [timestamp() - seconds_in(N, Period), timestamp()].

seconds_in(N, year)   -> N * 365 * 24 * 60 * 60;
seconds_in(N, month)  -> N * 31 * 24 * 60 * 60;
seconds_in(N, day)    -> N * 24 * 60 * 60;
seconds_in(N, hour)   -> N * 60 * 60;
seconds_in(N, min)    -> N * 60;
seconds_in(N, sec)    -> N.

bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} ->
            list_to_integer(N);
        {F,_Rest} ->
            F
    end.

timestamp() ->
    {Mega, Secs, _} = now(),
    Mega*1000000 + Secs.
