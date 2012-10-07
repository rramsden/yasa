-module(yasa_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ok = start_web_server(),
    ok = check_retentions(),
    yasa_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_web_server() ->
    Port = case application:get_env(yasa, port) of 
        {ok, P} -> P;
        undefined -> 8080
    end,
    Dispatch = [
            %% {Host, list({Path, Handler, Opts})}
            {'_', [
                    {[<<"api">>, action], yasa_http_api_handler, []},
                    {[<<"wsapi">>], bullet_handler, [{handler, yasa_ws_api_handler}]},
                    {[<<"assets">>, '...'], cowboy_http_static,
                     [{directory, {priv_dir, yasa, [<<"www/assets">>]}},
                      {mimetypes, [
                                    {<<".css">>, [<<"text/css">>]},
                                    {<<".js">>, [<<"application/javascript">>]},
                                    {<<".png">>, [<<"image/png">>]}]}]},
                    {[], yasa_default_handler, []},
                    {[<<"ws">>], yasa_default_handler, []}
                    ]}
            ],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    cowboy:start_listener(my_http_listener, 16,
                          cowboy_tcp_transport, [{port, Port}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ),
    ok.

%% @private check if the retentions are define in config file and make
%% sure the are not relatively primary
check_retentions() ->
    case application:get_env(yasa, retentions) of 
        undefined -> throw("Please define retentions");
        {ok, Rets} -> Rets
    end.
