%%%-------------------------------------------------------------------
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(yasa_rrd_server).
-include("rrd.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
    type,
    rrd
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Type, Key) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Type, Key], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Type, Key]) ->
    case yasa_rrd:load(Key) of
        {ok, RRD} ->
            {ok, #state{rrd = RRD, type = Type}};
        {error, not_found} ->
            {ok, #state{rrd = yasa_rrd:new(Type, Key), type = Type}}
    end.

handle_call({Type, Points}, _From, #state{rrd = RRD0} = State0) ->
    Module = module_for(Type),
    RRD1 = Module:insert(Points, RRD0),
    State1 = State0#state{rrd = RRD1},
    {reply, ok, State1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

module_for(gauge) -> yasa_metrics_gauge;
module_for(counter) -> yasa_metrics_counter.
