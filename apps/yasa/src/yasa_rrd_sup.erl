-module(yasa_rrd_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), 
        {Id, {Mod, start_link, Args}, permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Type, Key) ->
    supervisor:start_child(?MODULE, [Type, Key]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 10, 

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient, % restart child if it exits
                         % with something other than {'EXIT', normal}
    Shutdown = 2000,
    Type = worker,

    RRD = {'yasa_rrd_server', {'yasa_rrd_server', start_link, []},
                      Restart, Shutdown, Type, ['yasa_rrd_server']},

    {ok, {SupFlags, [RRD]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
