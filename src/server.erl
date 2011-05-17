%% @author Tobias Ericsson <tobiasericsson90@hotmail.com>
%% @author Andreas Hammar <andreashammar@gmail.com>
%% @author Gabriella Lundborg <gabriella_lundborg@hotmail.com>
%% @author Emma Rangert <emma.rangert@gmail.com>
%% @author John Reuterswärd <rojters@gmail.com>
%% @author Simon Young <youngen.simon@gmail.com>
%% @doc This is the main server that handles all the communication between
%% clients and clients to database the database.

-module(server).
-export([init/0, runtest/0, getGame/1]).
-include_lib("eunit/include/eunit.hrl").

%% @doc initiates the server
%% @spec init() -> server()

init() ->
    erlang:set_cookie(node(), earl_game_club),
    io:format("-----------------------------------------------~n", []),
    io:format("-----  Earl's Game Club server initiated  -----~n", []),
    io:format("-----------------------------------------------~n", []),
    io:format("         Host:   ~s~n", [net_adm:localhost()]),
    io:format("         Node:   ~s~n", [node()]),
    io:format("         Cookie: ~s~n", [erlang:get_cookie()]),
    io:format("-----------------------------------------------~n~n", []),
    
	register(srv, self()),
    register(db,spawn(database,init,[])),
    LoadList = loadGamesList(),
    GameList = spawnGameRooms(LoadList,[]),
	spawn(gameRoom_supervisor, init, [GameList, db]),
    db ! {setGamesList, GameList},
    server().

%% @doc receives messages and realays them to the db or start other processes to handle the
%% the message.

server() ->
    receive
		{setStatus, Pid, Alias,Status} ->  
		    io:format("Server: Received 'setStatus' request, forwarding to database~n", []),
		    db ! {setStatus, Pid, Alias, Status};
		{enterGameRoom, Alias, Origin, {GameModule, GameRoomPid}} ->
		    io:format("Server: Received 'enterGameRoom' request, forwarding to database~n", []),
			db ! {setStatus, Origin, Alias, [game, GameModule]},
		    GameRoomPid ! {newPlayer, Origin, Alias};
		{checkAlias, Alias, Origin} -> 
		    io:format("Server: Received 'checkAlias', forwarding to db~n", []),
			spawn(client_supervisor, init, [self(), Origin]),
			db ! {checkAlias, Alias, Origin};
		{quit, Pid} ->
		    io:format("Server: Received 'quit' from ~w, sending removal request to db~n", [Pid]),
		    db ! {remove,Pid};
		{debug, Msg} ->
		    io:format("~s~n", [Msg]);
		{getNumClients,Origin} ->
		    io:format("server recevied 'getNumClients'~n",[]),
		    db ! {getNumClients,Origin};
		{getSameStatus, Status, Origin} ->
		    io:format("Server recevied 'getSameStatus'~n", []),
		    db ! {getSameStatus, Status, Origin};
		{getAlias, Pid, Origin} -> 
			io:format("Server: received getAlias request, forwarding to database", []),
			db ! {getAlias, Pid, Origin};
		{getPid, Alias, Origin} ->
			io:format("Server: received getPid request, forwarding to database", []),
			db ! {getPid, Alias, Origin}
    end,
    server().

getGame(GameModule) ->
	db ! {getGame, GameModule, self()},
	receive
		{gameInfo, Game} ->
			Game
	end.

loadGamesList() ->
    case file:open("games.ini", read) of
	{ok, Device} ->
	    readLines(Device, []);
	{error, Reason} ->
	    {error, Reason}
    end.
readLines(File, Games) ->
    case io:get_line(File, "") of
	eof ->
	    Games;
	Line ->
	    [GameString | T] = string:tokens(Line, "\t\n"),
	    [GameName | _] = T,
	    Game = list_to_atom(GameString),
	    
	    io:format("Game: ~w - ~s loaded.~n", [Game, GameName]),
	    readLines(File, [{Game, GameName} | Games])
    end.


%% @doc Spawns a game room for all games in a list
%% @spec spawnGameRooms(List) -> ok
%% @hidden
spawnGameRooms([],GameList) -> GameList;
spawnGameRooms([{GameModule, DisplayName}|T],GameList) ->
    Pid = spawn(game_room, init, [GameModule, DisplayName]),
    spawnGameRooms(T,[{GameModule, DisplayName, Pid} | GameList]). 

% Test cases

%% @hidden

runtest() ->
    test(),
    init:stop().

