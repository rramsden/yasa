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
    key,
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
        {ok, {Type, RRD}} ->
            {ok, #state{rrd = RRD, type = Type}};
        {ok, _} ->
            {stop, type_mismatch};
        {error, not_found} ->
            Retentions = retentions_for_key(Key),
            {ok, #state{rrd = yasa_rrd:new(Retentions), type = Type}}
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

retentions_for_key(_Key) ->
    {ok, Retentions} = application:get_env(yasa, retentions),
    Retentions.

module_for(gauge) -> yasa_metrics_gauge;
module_for(counter) -> yasa_metrics_counter.
