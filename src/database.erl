%% @author Tobias Ericsson <tobiasericsson90@hotmail.com>
%% @author Andreas Hammar <andreashammar@gmail.com>
%% @author John Reuterswärd <rojters@gmail.com>
%% @author Simon Young <youngen.simon@gmail.com>
%% @doc this is the database that hold all the info of the clients currently
%% in the database.


-module(database).
-export([init/0, printClients/0, printNumClients/0]).

%% @doc initiates the database.
%% @spec init() -> database()

init() ->
    ets:new(clientTable,[set,named_table]),
    database().

%% @doc the database.

database() ->
    receive
	{getAll, Origin} ->
	    Origin ! ets:tab2list(clientTable);
	
	{getStatus, Pid, Origin} ->
	    X = ets:lookup(clientTable,Pid),
	    Origin ! X;
	{getAlias,Pid,Origin} ->
	    Origin ! {answer,getAlias(Pid)};
	
	{getSameStatus, Status, Origin} ->
	    X = ets:match(clientTable, {'$1', '$2', Status}),
	    Origin ! {statusList, X};  
	
	{getNumClients, Origin} ->
	    Origin ! ets:info(clientTable, size);
	
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

%% @doc Check if Alias is already in database, sends an answer to the asking process.

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

%% @doc prints all the clints in the database.
%% @spec printClients() -> {getAll,self()}

printClients() ->
    db ! {getAll, self()},
    receive
	ClientList -> 
	    io:format("~w~n", ClientList)
    end.

%% @doc prints the number of clients currently online.
%% @spec printNumClients() -> {getNumClients, self()}

printNumClients() ->
    db ! {getNumClients, self()},
    receive
	Num ->
	    io:format("~p~n", Num)
    end.

%% @doc returns the alias that belongs to Pid
%% @spec getAlias(Pid) -> Alias.

getAlias(Pid) ->
    ets:lookup_element(clientTable,Pid,2).
