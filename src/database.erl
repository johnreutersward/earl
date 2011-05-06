-module(database).
-export([init/0, printClients/0]).

init() ->
    ets:new(clientTable,[set,named_table]),
    database().
    

database() ->
    receive
		{client, getAll, Origin} ->
			Origin ! ets:tab2list(clientTable);

		{client,insert,{Pid,Alias,Status}} ->
			ets:insert(clientTable,{Pid,Alias,Status});
		
		{client, getStatus, Pid, Origin} ->
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

printClients() ->
	db ! {client, getAll, self()},
	receive
		ClientList -> 
			io:format("~w~n", ClientList)
	end.
