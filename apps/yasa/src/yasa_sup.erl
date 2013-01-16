-module(yasa_sup).
-behaviour(supervisor).

-include("yasa.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%===================================================================
%% API functions
%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%===================================================================
%% Supervisor callbacks
%%===================================================================

init([]) ->
    create_tables(),

    RestartStrategy = {one_for_one, 5, 10},
    Children = [
        ?CHILD(yasa_rrd_sup, supervisor)
    ],
    {ok, {RestartStrategy, Children}}.

%%===================================================================
%% Internal Functions
%%===================================================================

create_tables() ->
    Tables = [ {?PID_TABLE, [set, named_table, public, {read_concurrency, true}]} ],
    [maybe_create_table(ets:info(Name), Name, Opts) || {Name, Opts} <- Tables],
    ok.

maybe_create_table(undefined, Name, Opts) ->
    ets:new(Name, Opts);
maybe_create_table(_, _, _) ->
    ok.
