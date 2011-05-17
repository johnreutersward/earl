%% @author Tobias Ericsson <tobiasericsson90@hotmail.com>
%% @author Andreas Hammar <andreashammar@gmail.com>
%% @author Gabriella Lundborg <gabriella_lundborg@hotmail.com>
%% @author Emma Rangert <emma.rangert@gmail.com>
%% @author John Reuterswärd <rojters@gmail.com>
%% @author Simon Young <youngen.simon@gmail.com>
%% @doc this module contains all the functions that is related to the game room.

-module(game_room).
-export([init/2, handleInput/5, commandParser/5, sendChallange/2, printPlayers/1, sendToClient/2]).
-include_lib("eunit/include/eunit.hrl").

%% @doc initiates the game room
%% @spec init(Game,GameName) -> room(Game,GAmeName,PlayerList)

init(Game, GameName) ->
    srv ! {debug, "New game room spawned."},
    srv ! {getSameStatus, [game, Game], self()},
    receive
	{statusList, PlayersList} ->
	    room(Game, GameName, PlayersList)
    end.
	
%% @doc the game room, it handles data between users.

room(Game, GameName, PlayerList) ->
    receive
	{newPlayer, Pid, Alias} ->
	    srv ! {debug, "New player added to "++GameName++" room"},
	    Pid ! {message, GameName, "Welcome to "++GameName++" game room!\n\n"
		   ++"Available commands are: /players, /quit, /challenge\n"},
	    sendMessage(PlayerList, "", Alias++" has joined the room"),
	    NewPlayerList = [{Pid, Alias, [game, Game]} | PlayerList];
	{quitPlayer, Pid, Alias} ->
	    NewPlayerList = lists:keydelete(Pid, 1, PlayerList),
	    Pid ! {back},
	    srv ! {setStatus, Pid, Alias, [main]},
	    sendMessage(PlayerList, "", Alias++" has left the room.");
	{input, Origin, Alias, Input} ->
	    srv ! {debug, "Handle player input "++GameName++" room."},
	    spawn(game_room,handleInput, [self(), Input, Origin, Alias, PlayerList]),
	    NewPlayerList = PlayerList;	
	{challenge, Aliases, Origin} ->
		srv ! {debug, "Challenging another player."},
		spawn(game_room, sendChallange, [Aliases, Origin]),
		NewPlayerList = PlayerList;
	{initiateGame, Players} ->
		Players2 = [{Pid, Alias, [game, Game]} || {Pid, Alias} <- Players],
		srv ! {setStatus, Players2},
		NewPlayerList = lists:filter(fun({P, _, _}) -> lists:keymember(P, 1, Players) == false end, PlayerList),
		spawn(gameAPI, init, [Game, Players])

    end,
    room(Game, GameName, NewPlayerList).

%% @doc handles the input depending on if it starts with "/" or not
%% @hidden

handleInput(RoomPid, Input, Origin, Alias, PlayerList) ->
    if 
	[hd(Input)] == "/" ->
	    commandParser(RoomPid,Input,Origin,Alias, PlayerList);
	true ->
%	    spawn(game_room,sendMessage,[PlayerList,Alias,Input])
	    ok
    end.

%% @doc this function decides which command the user wants to input.
%% @hidden
	
commandParser(RoomPid, [_ | Input], Origin, Alias, PlayerList) ->
    [Command | Params] = string:tokens(Input, " "),
    case Command of
	"challenge" ->
	   RoomPid ! {challenge, Params, {Origin, Alias}}; 
	"quit" ->
	    RoomPid ! {quitPlayer, Origin, Alias};
	"players" ->
	    AliasList = lists:sort([X || {_, X, _} <- PlayerList]),	
	    Origin ! {printPlayers, AliasList};
	"Nyan" ->
		8/0;
	_ ->
	    Origin ! {message, "", "Invalid Command."}
    end.

%% @doc prints all the players in the game room

printPlayers([]) -> ok;
printPlayers([Alias | AliasList]) ->
    io:format("~w ", [Alias]),
    printPlayers(AliasList).

%% @doc sends a message to all the users in the game room.
%% @hidden

sendMessage([], _, _) ->
    ok;
sendMessage([{H, User, _} | T], Alias, Message) ->
    if 
	User == Alias ->
	    ok;
	true ->
	    H ! {message, Alias, Message}
    end,
    sendMessage(T, Alias, Message).

sendToClient(Pid, Message) ->
    Pid ! {directMessage, Message}.

sendChallange(Aliases, Origin) ->
	Alias = hd(Aliases),
	srv ! {getPid, Alias, self()},
	receive
		{returnPid, nomatch} ->
			sendToClient(Origin, "ERROR: No such player available\n");
		{returnPid, Pid} ->
			Pid ! {challange, Origin}
	end.
