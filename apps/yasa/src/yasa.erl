-module(yasa).
-export([counter/2, gauge/2, get/3]).

%%===================================================================
%% Public API
%%===================================================================

counter(Key, Points) ->
    {ok, Pid} = find_or_create(gauge, Key),
    gen_server:call(Pid, {gauge, Points}).

gauge(Key, Points) ->
    {ok, Pid} = find_or_create(counter, Key),
    gen_server:call(Pid, {counter, Points}).

get(Key, Start, End) ->
    {ok, Pid} = case find(Key) of
        {error, not_found} ->
            {error, not_found};
        Ref ->
            Ref
    end,
    gen_server:call(Pid, {get, Start, End}).

%%===================================================================
%% Internal
%%===================================================================

find_or_create(Type, Key) ->
    case find(Key) of
        {ok, Ref} ->
            {ok, Ref};
        {error, not_found} ->
            create(Type, Key)
    end.

find(Key) ->
    case yasa_pid_store:lookup(Key) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_found} ->
            {error, not_found}
    end.

create(Type, Key) ->
    {ok, Pid} = Reply = yasa_rrd_sup:start_child(Type, Key),
    yasa_pid_store:insert(Key, Pid),
    Reply.
