%% @author Simon Young, Tobias.
%% @doc The server that handles all the traffic between clients and also stores information
%% about there status. The server also enables clients to play games against eachother.

-module(server).
-export([init/0,server/1,checkAlias/3]).

server(ClientTable) ->
    io:format("Server pid~w~n", [self()]),
    io:format("ClientTable~w~n", [ClientTable]),
    receive
	{status,Pid,Alias,Status} -> ets:insert(ClientTable, {Pid,Alias,Status});			      
	{checkAlias,Pid,Alias} -> spawn(server,checkAlias,[Pid,Alias,ClientTable])		       	      			 
    end,
    server(ClientTable).

%% @doc starts up the server.
init() ->
    ClientTable = ets:new(clientTable,[set]),
ets:insert(ClientTable, {self(),"youngen",[main]}),
    global:register_name(mainServer_PID,spawn(server,server,[ClientTable])).

%% @doc Checks if the Alias is already in use.
checkAlias(Pid,Alias,ClientTable) ->
    Answer = ets:match(ClientTable,{'$1',Alias,'_'}),
    if
	Answer == [] -> Pid ! aliasTrue;
	true -> Pid ! aliasFalse
    end.
	    
    
    
	

