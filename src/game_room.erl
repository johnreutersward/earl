%% @author Tobias.Ericsson.0701@student.uu.se
%% @author Andreas.Hammar.5781@student.uu.se
%% @author Gabriella.Lundborg.6304@student.uu.se
%% @author Emma.Rangert.2142@student.uu.se
%% @author John.Reuterswärd.8971@student.uu.se
%% @author Simon.Young.0963@student.uu.se
%% @doc this module contains all the functions that is related to the game room.

-module(game_room).
-export([init/2, handleInput/6, commandParser/6, sendChallenge/4, printPlayers/1, sendToClient/2]).
-include_lib("eunit/include/eunit.hrl").

%% @doc initiates the game room
%% @spec init(Game,GameName) -> room(Game,GameName,PlayerList)

init(Game, GameName) ->
    srv ! {debug, "New game room spawned."},
    srv ! {getSameStatus, [game, Game], self()},
    receive
	{statusList, PlayersList} ->
	    room(Game, GameName, PlayersList, [])
    end.

%% @doc the game room, it handles data between users.
%% @spec room(Game, GameName, PlayrList, HighScore) -> ok

room(Game, GameName, PlayerList, HighScore) ->
    receive
	{newPlayer, Pid, Alias} ->
	    srv ! {debug, "New player added to "++GameName++" room"},
	    Pid ! {message, GameName, "Welcome to "++GameName++" game room!\n\n"
		   ++"Available commands are: /players, /quit, /challenge, /high-score\n"},
	    sendMessage(PlayerList, "", Alias++" has joined the room"),
	    NewPlayerList = [{Pid, Alias, [game, Game]} | PlayerList],
	    NewHighScore = HighScore;
	{quitPlayer, Pid, Alias} ->
	    NewPlayerList = lists:keydelete(Pid, 1, PlayerList),
	    Pid ! {back},
	    srv ! {setStatus, Pid, Alias, [main]},
	    sendMessage(PlayerList, "", Alias++" has left the room."),
	    NewHighScore = HighScore; 
	{input, Origin, Alias, Input} ->
	    srv ! {debug, "Handle player input "++GameName++" room."},
	    spawn(game_room,handleInput, [self(), Input, Origin, Alias, PlayerList, HighScore]),
	    NewPlayerList = PlayerList,
	    NewHighScore = HighScore;
	{finish, WinnerAlias} ->
	    if 
		HighScore == [] ->
		    NewHighScore = [{1, WinnerAlias}];
		true ->
		    NewHighScore = lists:reverse(lists:keysort(1,(updateHighScore(WinnerAlias, HighScore))))
	    end,
	    NewPlayerList = PlayerList;
	{challenge, Aliases, Origin} ->
	    srv ! {debug, "Challenging another player."},
	    spawn(game_room, sendChallenge, [Aliases, Origin, self(), PlayerList]),
	    NewPlayerList = PlayerList,
	    NewHighScore = HighScore;
	{initiateGame, Players} ->
	    srv ! {debug, "GameRoom received initiateGame, attempting to start game module"},
	    srv ! {setStatus, Players},
	    NewPlayerList = lists:filter(fun({P, _, _}) -> lists:keymember(P, 1, Players) == false end, PlayerList),
	    GamePid = spawn(gameAPI, init, [Game, Players, self()]),
	    playersToGameMode(GamePid, Players),
	    NewHighScore = HighScore
    end,
    room(Game, GameName, NewPlayerList, NewHighScore).

%% @doc handles the input depending on if it starts with "/" or not
%% @hidden

handleInput(RoomPid, Input, Origin, Alias, PlayerList, HighScore) ->
    if 
	[hd(Input)] == "/" ->
	    commandParser(RoomPid,Input,Origin,Alias, PlayerList, HighScore);
	true ->
	    ok
    end.

%% @doc this function decides which command the user wants to input.
%% @hidden

commandParser(RoomPid, [_ | Input], Origin, Alias, PlayerList, HighScore) ->
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
	"high-score" ->
	    Origin ! {printHighScore, HighScore};
	_ ->
	    Origin ! {message, "", "Invalid Command."}
    end.

%% @doc prints all the players in the game room
%% @spec printPlayers(AliasList) -> ok

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

%% @doc Makes a player go into game mode. 
%% @spec playersToGameMode(GamePid, Players) -> ok

playersToGameMode(_, []) ->
    ok;
playersToGameMode(GamePid, [{Pid, Alias} | Players]) ->
    srv ! {debug, "Making "++Alias++" go into game mode"},
    Pid ! {game, GamePid},
    playersToGameMode(GamePid, Players).

%% @doc Adds the highscore of a new player, if a player already exists its highscore will be increased by one.
%% @spec updateHighScore(WinnerAlias, HighScore) -> HighScore

updateHighScore(WinnerAlias, []) -> 
    [{1, WinnerAlias}];
updateHighScore(WinnerAlias, [{Score, Alias} | HighScore]) ->
    if
	WinnerAlias == Alias ->
	    [{Score + 1, Alias} | HighScore];
	true ->
	    [{Score, Alias} | updateHighScore(WinnerAlias, HighScore)]
    end.

%% @doc Sends a message to a client with the specified pid. 
%% @spec sendToClient(Pid, Message) -> ok

sendToClient(Pid, Message) ->
    Pid ! {message, "", Message}.

%% @doc Sends a challenge to a specified client.
%% @spec sendChallenge(Alias, Origin, GameRoomPid) -> ok

sendChallenge(Aliases, {OriginPid, OriginAlias}, GameRoomPid, PlayerList) ->
	Alias = hd(Aliases),
	case lists:keyfind(Alias, 2, PlayerList) of
		{Pid, _, _} ->
		    Pid ! {challenge, GameRoomPid, {OriginPid, OriginAlias}};
		false ->
			sendToClient(OriginPid, "ERROR: No such player available\n")
	end.

