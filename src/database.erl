-module(database).
-export([init/0]).

init() ->
    ets:new(clientTable,[set,named_table]),
    database().
    

database() ->
    receive
	{client,insert,{Pid,Alias,Status}} ->
	    ets:insert(clientTable,{Pid,Alias,Status});
	{client, status, Pid, Origin} ->
	    X = ets:lookup(clientTable,Pid),
	    Origin ! X;
	{client,setStatus, Pid, Alias, Status} ->
	    ets:insert(clientTable,{Pid,Alias,Status});
	{checkAlias,Origin,Alias} ->
	    checkAlias(Origin,Alias);
	{remove,Pid} ->
	    ets:delete(clientTable,Pid)
    end,
    database().


checkAlias(Origin, Alias) ->
    Answer = ets:match(clientTable, {'$1', Alias, '_'}),
    if
		Answer == [] -> 
			Origin ! aliasTrue;
		true -> 
			Origin ! aliasFalse
    end.
