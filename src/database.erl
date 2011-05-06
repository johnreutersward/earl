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
			io:format("Database: Inserting ~s as ~w in client table~n", Alias, Status),
			ets:insert(clientTable,{Pid,Alias,Status});
		
		{client, getStatus, Pid, Origin} ->
		    X = ets:lookup(clientTable,Pid),
		    Origin ! X;
		
		{client,setStatus, Pid, Alias, Status} ->
		    ets:insert(clientTable,{Pid,Alias,Status});
		
		{checkAlias,Origin,Alias} ->
		    checkAlias(Origin,Alias);
		
		{remove,Pid} ->
			io:format("Database: Removing ~w from client table~n", Pid),
			ets:delete(clientTable,Pid)
    end,
    database().


checkAlias(Origin, Alias) ->
    io:format("Database: Checking if alias ~s exists in client table~n", Alias),
	Answer = ets:match(clientTable, {'$1', Alias, '_'}),
    if
		Answer == [] -> 
			io:format("Database: Alias ~s does not exist in client table~n", Alias),
			Origin ! aliasTrue;
		true -> 
			io:format("Database: Alias ~s already exists in client table~n", Alias),
			Origin ! aliasFalse
    end.

printClients() ->
	db ! {client, getAll, self()},
	receive
		ClientList -> 
			io:format("~w~n", ClientList)
	end.
