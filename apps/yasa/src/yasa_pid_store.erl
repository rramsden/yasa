% @doc This process stores key-to-pid mappings in its state. It is used
-module(yasa_pid_store).

-include("yasa.hrl").

%% API
-export([insert/2,
         lookup/1,
         delete/1
         ]).

%%%===================================================================
%%% API
%%%===================================================================

-spec lookup(term()) -> {ok, pid()}|{error, not_found}.
lookup(Key) ->
    case ets:lookup(?PID_TABLE, Key) of
        [{Key, Pid}] ->
            {ok, Pid};
        _ ->
            {error, not_found}
    end.

-spec insert(term(), pid()) -> true|false.
insert(Key, Pid) ->
    ets:insert(?PID_TABLE, {Key, Pid}).

-spec delete(term()) -> true|false.
delete(Key) ->
    ets:delete(?PID_TABLE, Key).
