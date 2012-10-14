-module(yasa).
-export([
    counter/2,
    gauge/2,
    get/3,
    walk_directory_tree/1,
    keys/0]).

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

keys() ->
    Root = [code:priv_dir(yasa), "/storage/*"],
    lists:flatten([walk_directory_tree(Root)]).

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

walk_directory_tree(Root) ->
    lists:map(fun(Elem) ->
        case filelib:is_dir(Elem) of
            true ->
                walk_directory_tree([Elem, "/*"]);
            false ->
                get_key_from_path(Elem)
        end
    end, filelib:wildcard(Root)).

get_key_from_path(Path) ->
    FlatPath = lists:flatten(Path), 
    case re:run(FlatPath, <<"storage\\/(?<NAME>.+)\.yasa$">>, [{capture, ['NAME'], binary}]) of
        {match, [Name]} ->
            re:replace(Name, "\\/", ".", [global, {return, binary}]);
        nomatch ->
            []
    end.
