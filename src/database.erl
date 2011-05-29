%% @author Tobias.Ericsson.0701@student.uu.se
%% @author Andreas.Hammar.5781@student.uu.se
%% @author Gabriella.Lundborg.6304@student.uu.se
%% @author Emma.Rangert.2142@student.uu.se
%% @author John.Reutersward.8971@student.uu.se
%% @author Simon.Young.0963@student.uu.se
%% @doc this is the database that holds all the information of the clients currently
%% in the database.

-module(database).
-export([init/0, clients/0, printClients/1, printNumClients/0]).

%% @doc Initiates one database for the connected clients, one database for the available games 
%% and then initiates the database function. 
%% @spec init() -> database()

init() ->
    ets:new(clientTable, [set,named_table]),
    ets:new(gamesTable, [set, named_table]),
    database().

%% @doc The database function receives messages from the server and pass them on to the requesting process. 
%% @spec database() -> database()

database() ->
    receive
	{getAll, Origin} ->
	    Origin ! ets:tab2list(clientTable);
	
	{getStatus, Pid, Origin} ->
	    X = ets:lookup(clientTable,Pid),
	    Origin ! X;
	
	{getAlias, Pid, Origin} ->
	    Origin ! {alias, ets:lookup_element(clientTable,Pid,2)};

	{getPid, Alias, Origin} ->
		Pids = ets:match(clientTable, {'$1', Alias, '_'}),
		case Pids of
			[] -> 
				Origin ! {returnPid, nomatch};
			_ ->
				Origin ! {returnPid, hd(hd(Pids))}
		end;
	
	{getSameStatus, Status, Origin} ->
	    X = ets:match(clientTable, {'$1', '$2', Status}),
	    Origin ! {statusList, X};  
	
	{getNumClients, Origin} ->
	    Origin ! {numClients, ets:info(clientTable, size)};
	
	{setStatus, Pid, Alias, Status} ->
	    srv ! {debug, "Database: Setting status for "++Alias},
	    ets:insert(clientTable,{Pid,Alias,Status});
	{setStatus, List} ->
		srv ! {debug, "Database: Setting status for list of players"},
		ets:insert(clientTable, List);

	{checkAlias, Alias, Origin} ->
	    checkAlias(Alias, Origin);
	
	{remove,Pid} ->
	    srv ! {debug, "Database: Removing client from client table"},
	    ets:delete(clientTable,Pid);


	{getGame, GameModule, Origin} ->
		srv ! {debug, "Database: Returning game information"},
		Origin ! {gameInfo, ets:lookup(gamesTable, GameModule)};

	{setGamesList, List} ->
	    srv ! {debug, "Database: Setting Games Table."},
	    ets:delete_all_objects(gamesTable),
	    ets:insert(gamesTable, List);
	
	{getGamesList, Origin} ->
	    srv ! {debug, "Database: Returning list of games to clienthandler"},
	    Origin ! {gamesList, ets:tab2list(gamesTable)}
		
    end,
    database().

%% @doc The function checks whether Alias is already in the clientTable database and then sends an answer to the asking process.
%% @hidden

checkAlias(Alias, Origin) ->
    srv ! {debug, "Database: Checking if alias '"++Alias++"' exists in client table"},
    Answer = ets:match(clientTable, {'$1', Alias, '_'}),
    if
	Answer == [] -> 
	    srv ! {debug, "Database: Alias '"++Alias++"' does not exist in client table, returning valid"},
		Origin ! {aliasValid};
	true -> 
	    srv ! {debug, "Database: Alias '"++Alias++"' alreandy exists in client table"},
	    Origin ! aliasInvalid 
    end, 
    ok.

%% @doc This function prints all the clients in the database.
%% @spec clients() -> printClients(ClientList)
%% @hidden

clients() ->
    db ! {getAll, self()},
    receive
	ClientList -> 
	    printClients(ClientList)
    end.

%% @doc This function prints all the clients in the database.
%% @hidden

printClients([]) ->
    done;
printClients([{Pid, Alias, Status} | T]) ->
    io:format("{~w, ~s, ~w}~n", [Pid, Alias, Status]),
    printClients(T).

%% @doc This function prints the number of clients currently online.
%% @spec printNumClients() -> {getNumClients, self()}

printNumClients() ->
    db ! {getNumClients, self()},
    receive
	Num ->
	    io:format("~p~n", Num)
    end.

