-module(yasa).
-export([
    update/2,
    fetch/3,
    walk_directory_tree/1,
    keys/0]).

%%===================================================================
%% Public API
%%===================================================================

update(Key, Points) ->
    case yasa_pid_store:lookup(Key) of
        {ok, Pid} ->
            yasa_rrd_server:update(Pid, Points);
        {error, Reason} ->
            {error, Reason}
    end.

fetch(Key, Start, End) ->
    {ok, Pid} = case yasa_pid_store:lookup(Key) of
        {error, not_found} ->
            {error, not_found};
        Ref ->
            Ref
    end,
    yasa_rrd:lookup(Pid, {Start, End}).

keys() ->
    Root = [code:priv_dir(yasa), "/storage/*"],
    lists:flatten([walk_directory_tree(Root)]).

%%===================================================================
%% Internal
%%===================================================================

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
