-module(game_room).
-export([init/2, room/3, handleInput/3, commandParser/1, sendMessage/3]).
-include_lib("eunit/include/eunit.hrl").


init(Game, GameName) ->
	srv ! {debug, "New game room spawned."},
	srv ! {getSameStatus, [game, Game], self()},
	receive
		{statusList, PlayersList} ->
			room(Game, GameName, PlayersList)
	end
.

room(Game, GameName, PlayerList) ->
	receive
		{newPlayer, Pid, Alias} ->
			srv ! {debug, "New player added to "++GameName++" room"},
			Pid ! {message, "Welcome to "++GameName++" game room!"},
			sendMessage(PlayerList, "", Alias++" has joined the room"),
			[{Pid, Alias, [game, Game]} | PlayerList];
		{quitPlayer, Pid, Alias} ->
			NewPlayerList = lists:keydelete(Pid, 1, PlayerList),
			srv ! {setStatus, Pid, Alias, [main]},
			sendMessage(PlayerList, "", Alias++" has left the room.");
		{input, Pid, Alias, Input} ->
			handleInput(Input, Pid, Alias)
	end,
	room(Game, GameName, PlayerList)
.

handleInput(Input, Pid, Alias) ->
	if 
		[hd(Input)] == "/" ->
			Command = commandParser(Input);
		true ->
			{message, Input}
	end
	.
commandParser([_, Input]) ->
	[Command | Params] = string:split(Input, " "),
	case Command of
		"challenge" ->
			{challenge, Params};
		_ ->
			{invalid}
	end
	.

sendMessage([], Alias, Message) ->
	ok;
sendMessage([{H, _, _} | T], Alias, Message) ->
	H ! {message, Alias, Message},
	sendMessage(T, Alias, Message).
