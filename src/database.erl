-module(database).
-export([init/0, printClients/0]).

init() ->
    ets:new(clientTable,[set,named_table]),
    database().
    

database() ->
	receive
		{getAll, Origin} ->
			Origin ! ets:tab2list(clientTable);

		{getStatus, Pid, Origin} ->
		    X = ets:lookup(clientTable,Pid),
		    Origin ! X;
		
		{setStatus, Pid, Alias, Status} ->
			srv ! {debug, "Database: Setting status for "++Alias},
			ets:insert(clientTable,{Pid,Alias,Status});
		
		{checkAlias, Alias, Origin} ->
		    checkAlias(Alias, Origin);
		
		{remove,Pid} ->
			srv ! {debug, "Database: Removing client from client table"},
			ets:delete(clientTable,Pid)
    end,
    database().


checkAlias(Alias, Origin) ->
	srv ! {debug, "Database: Checking if alias '"++Alias++"' exists in client table"},
	Answer = ets:match(clientTable, {'$1', Alias, '_'}),
    if
		Answer == [] -> 
			srv ! {debug, "Database: Alias '"++Alias++"' does not exist in client table, returning valid"},
			Origin ! aliasValid;
		true -> 
			srv ! {debug, "Database: Alias '"++Alias++"' already exists in client table"},
			Origin ! aliasInvalid 
	end, 
	ok.

printClients() ->
	db ! {getAll, self()},
	receive
		ClientList -> 
			io:format("~w~n", ClientList)
	end.
