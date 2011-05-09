%% @author Tobias Ericsson <tobiasericsson90@hotmail.com>
%% @author Andreas Hammar <andreashammar@gmail.com>
%% @author John Reuterswärd <rojters@gmail.com>
%% @author Simon Young <youngen.simon@gmail.com>
%% @doc This is the main server that handles all the communication between
%% clients and clients to database the database.

-module(server).
-export([init/0,server/0,runtest/0,spawnGameRooms/1]).
-include_lib("eunit/include/eunit.hrl").

%% @doc initiates the server
%% @spec init() -> server()

init() ->
	erlang:set_cookie(node(), earl_game_club),
	register(srv, self()),
    register(db,spawn(database,init,[])),
	spawnGameRooms([{glhf,"GLHF"},{tictactoe,"Tic Tac Toe"}]),
    io:format("-----------------------------------------------~n", []),
    io:format("------  Earl Game Club server initiated  ------~n", []),
    io:format("-----------------------------------------------~n", []),
    io:format("         Host:   ~s~n", [net_adm:localhost()]),
    io:format("         Node:   ~s~n", [node()]),
    io:format("         Cookie: ~s~n", [erlang:get_cookie()]),
    io:format("-----------------------------------------------~n~n", []),
    server().

%% @doc receives messages and realays them to the db or start other processes to handle the
%% the message.

server() ->
    receive
	{setStatus, Pid, Alias,Status} ->  
	    io:format("Server: Received 'setStatus' request, forwarding to database~n", []),
	    db ! {setStatus, Pid, Alias, Status};
	{enterGameRoom, Origin,Game} ->
	    io:format("Server: Received 'enterGameRoom' request, forwarding to database~n", []),
	    db ! {getAlias, Origin,self()},
	    receive
		{answer,Answer} -> 
		    db ! {setStatus, Origin, Answer, [game, Game]}
	    end,
	    Game ! {newPlayer, Origin, Answer};
	{checkAlias, Alias, Origin} -> 
	    io:format("Server: Received 'checkAlias', forwarding to db~n", []),
	    db ! {checkAlias, Alias, Origin};	       	      
	{quit, Pid} ->
	    io:format("Server: Received 'quit' from pid, sending removal request to db~n", []),
	    db ! {remove,Pid};
	{debug, Msg} ->
	    io:format("~s~n", [Msg]);
	{getNumClients,Origin} ->
	    io:format("server recevied 'getNumClients'~n",[]),
	    db ! {getNumClients,Origin};
	{getSameStatus, Status, Origin} ->
		io:format("Server recevied 'getSameStatus'~n", []),
		db ! {getSameStatus, Status, Origin}
    end,
    server().

%% HELP FUNCTIONS %%

%% @doc Spawns a game room for all games in a list
%% @spec spawnGameRooms(List) -> ok

spawnGameRooms([]) -> ok;
spawnGameRooms([{GameModule, DisplayName}|T]) ->
	register(GameModule, spawn(game_room, init, [GameModule, DisplayName])),
	spawnGameRooms(T). 

% Test cases

runtest() ->
    test(),
    init:stop().

