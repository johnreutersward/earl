-module(game_room).
-export([init/2, room/3, handleInput/5, commandParser/4, sendMessage/3, printPlayers/1]).
-include_lib("eunit/include/eunit.hrl").


init(Game, GameName) ->
    srv ! {debug, "New game room spawned."},
    srv ! {getSameStatus, [game, Game], self()},
    receive
	{statusList, PlayersList} ->
	    room(Game, GameName, PlayersList)
    end.
	

room(Game, GameName, PlayerList) ->
    receive
	{newPlayer, Pid, Alias} ->
	    srv ! {debug, "New player added to "++GameName++" room"},
	    Pid ! {message, "Welcome to "++GameName++" game room!"},
	    sendMessage(PlayerList, "", Alias++" has joined the room"),
	    NewPlayerList = [{Pid, Alias, [game, Game]} | PlayerList];
	{quitPlayer, Pid, Alias} ->
	    NewPlayerList = lists:keydelete(Pid, 1, PlayerList),
	    srv ! {setStatus, Pid, Alias, [main]},
	    sendMessage(PlayerList, "", Alias++" has left the room.");
	{input, Pid, Alias, Input} ->
	    srv ! {debug, "Handle player input "++GameName++" room"},
	    spawn(game_room,handleInput, [self(), Input, Pid, Alias, PlayerList]),
	    NewPlayerList = PlayerList		
    end,
    room(Game, GameName, NewPlayerList).

handleInput(RoomPid, Input, Pid, Alias, PlayerList) ->
    if 
	[hd(Input)] == "/" ->
	    Command = commandParser(Input,Pid,Alias, PlayerList);
	true ->
	    sendMessage(PlayerList,Alias,Input)
    end
	.
commandParser([_, Input], Pid, Alias, PlayerList) ->
    [Command | Params] = string:split(Input, " "),
    case Command of
	"challenge" ->
	    {challenge, Params};
	"quit" ->
	    grm ! {quitPlayer, Pid, Alias};
	"players" ->
	    AliasList = lists:sort([X || {_, X, _} <- PlayerList]),	
	    printPlayers(AliasList);
	_ ->
	    {invalid}
    end.

printPlayers([]) -> ok;
printPlayers([Alias | AliasList]) ->
    io:format("~w ", [Alias]),
    printPlayers(AliasList).

sendMessage([], _, _) ->
    ok;
sendMessage([{H, User, _} | T], Alias, Message) ->
    if 
	User == Alias ->
	    sendMessage(T,Alias,Message);
	true ->
	    H ! {message, Alias, Message}
    end,
    sendMessage(T, Alias, Message).
