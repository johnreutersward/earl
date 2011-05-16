%% @author Tobias Ericsson <tobiasericsson90@hotmail.com>
%% @author Andreas Hammar <andreashammar@gmail.com>
%% @author Gabriella Lundborg <gabriella_lundborg@hotmail.com>
%% @author Emma Rangert <emma.rangert@gmail.com>
%% @author John Reuterswärd <rojters@gmail.com>
%% @author Simon Young <youngen.simon@gmail.com>
%% @doc this is the database that hold all the info of the clients currently
%% in the database.


-module(database).
-export([init/0, clients/0, printClients/1, printNumClients/0]).

%% @doc initiates the database.
%% @spec init() -> database()

init() ->
    ets:new(clientTable, [set,named_table]),
    ets:new(gamesTable, [set, named_table]),
    database().

%% @doc the database.
%% @spec database() -> database()

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
	    Origin ! {numClients, ets:info(clientTable, size)};
	
	{setStatus, Pid, Alias, Status} ->
	    srv ! {debug, "Database: Setting status for "++Alias},
	    ets:insert(clientTable,{Pid,Alias,Status});
	
	{checkAlias, Alias, Origin} ->
	    checkAlias(Alias, Origin);
	
	{remove,Pid} ->
	    srv ! {debug, "Database: Removing client from client table"},
	    ets:delete(clientTable,Pid);
	
	{setGamesList, List} ->
	    srv ! {debug, "Database: Setting Games Table."},
	    ets:delete_all_objects(gamesTable),
	    ets:insert(gamesTable, List);
	
	{getGamesList, Origin} ->
	    srv ! {debug, "Database: Returning list of games to clienthandler"},
	    Origin ! {gamesList, ets:tab2list(gamesTable)}
		
    end,
    database().

%% @doc Check if Alias is already in database, sends an answer to the asking process.
%% @hidden

checkAlias(Alias, Origin) ->
    srv ! {debug, "Database: Checking if alias '"++Alias++"' exists in client table"},
    Answer = ets:match(clientTable, {'$1', Alias, '_'}),
    if
	Answer == [] -> 
	    srv ! {debug, "Database: Alias '"++Alias++"' does not exist in client table, returning valid"},
		Origin ! {aliasValid, self()},
		receive
			{clientPid, ClientPid} ->
				spawn(client_handler, ping, [Origin, ClientPid])
		after 10000 ->
				srv ! {debug, "Database: Failed to recieve clientPid from Client Handler."}
		end;
	true -> 
	    srv ! {debug, "Database: Alias '"++Alias++"' alreandy exists in client table"},
	    Origin ! aliasInvalid 
    end, 
    ok.

%% @doc prints all the clints in the database.
%% @spec clients() -> printClients(ClientList)
%% @hidden

clients() ->
    db ! {getAll, self()},
    receive
	ClientList -> 
	    printClients(ClientList)
    end.

%% @doc prints all the clients in the database
%% @hidden

printClients([]) ->
    done;
printClients([{Pid, Alias, Status} | T]) ->
    io:format("{~w, ~s, ~w}~n", [Pid, Alias, Status]),
    printClients(T).

%% @doc prints the number of clients currently online.
%% @spec printNumClients() -> {getNumClients, self()}

printNumClients() ->
    db ! {getNumClients, self()},
    receive
	Num ->
	    io:format("~p~n", Num)
    end.

%% @doc returns the alias that belongs to Pid
%% @spec getAlias(Pid) -> Alias

getAlias(Pid) ->
    ets:lookup_element(clientTable,Pid,2).
