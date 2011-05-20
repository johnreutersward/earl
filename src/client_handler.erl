%% @author Tobias Ericsson <tobiasericsson90@hotmail.com>
%% @author Andreas Hammar <andreashammar@gmail.com>
%% @author Gabriella Lundborg <gabriella_lundborg@hotmail.com>
%% @author Emma Rangert <emma.rangert@gmail.com>
%% @author John Reuterswärd <rojters@gmail.com>
%% @author Simon Young <youngen.simon@gmail.com>
%% @doc This module has all the functions that the client needs on the server side.
%% This functions gives the user all the information and ability to interact with other users
%% and the server.

-module(client_handler).
-export([init/1, runtest/0, gameRoom/4,receiver/3]).
-include_lib("eunit/include/eunit.hrl").

%% @doc initiates the client handler.
%% @spec init(ClientPid) -> create_alias(ClientPid)

init(ClientPid) ->
    io:format("~n-- Welcome to Earl's Game Club!~n", []),
    numConnected(),
    create_alias(ClientPid).

%% @doc creates a user, when the client comes online.
%% @spec create_alias(ClientPid) -> main_menu(ClientPid)
%% @hidden

create_alias(ClientPid) ->
    io:format("Input a Username: ", []),
    Alias = getInput(),
    srv ! {checkAlias, Alias, self()},
    io:format("Handler: Waiting for server confirmation~n", []),
    receive
		{aliasValid} ->	
	    srv ! {setStatus, self(), Alias, [main]},
	    main_menu(ClientPid,Alias);
        aliasInvalid -> 
	    io:format("Alias is already in use, please choose another Alias~n", []),
	    create_alias(ClientPid)
    end.

%% @doc this is the main menu that the user sees upon entering the server.
%% from this function the user can enter game_menu() or quit().

main_menu(ClientPid, Alias) ->
    io:format("~n --Main Menu-- ~n", []),
    io:format("1 - Select game ~n", []),
    io:format("2 - Show statistics ~n", []),
    io:format("3 - Help ~n", []),
    io:format("4 - Quit ~n?> ", []),
    
    case(getInput()) of
	"1" ->
	    game_menu(Alias),
	    main_menu(ClientPid, Alias);
	"2" ->
	    numConnected(),
		main_menu(ClientPid,Alias);
	"3" ->
	    help(ClientPid,Alias);
	"4" ->
	    quit(ClientPid);
	_ ->
		io:format("~nIllegal command~n",[]),
	    main_menu(ClientPid,Alias)
    end.

%% @doc prints out the game menu for the user, and enables the user to connect to a game room.

game_menu(Alias) ->
	db ! {getGamesList, self()},
	receive
		{gamesList, GameList} ->
			game_menu(GameList, 1, Alias, GameList)
	end.

game_menu([], Num, Alias, GameList) -> 
    IntString = integer_to_list(Num),
    io:format(IntString ++ " - Back to Main Menu~n?> ", []),
    Input = getNumber(), 
    case(Input) of
	{error} ->
	    io:format("Illegal command!~n", []),
	    game_menu(Alias);
	_ when Input > 0 , Input < Num ->
	    io:format("~w~n", [Input]),
	    Game = lists:nth(Input, GameList),
	    GameRoomInputPid = spawn(client_handler, gameRoom, [Game, self(), Alias, 0]),
	    State = receiver(GameList, 1, Alias),
	    exit(GameRoomInputPid, normal),
	    case State of 
		{game, GamePid} ->
		    gameMode(GamePid);
		_ -> 
		    ok
	    end;
	    
	_ when Input == Num ->
	    ok;	
	_ -> 
	    io:format("Illegal command!~n", []),
	    game_menu(GameList, 1, Alias,GameList)
    end;

game_menu([{_, DisplayName,_} | GameListIter], Num, Alias, GameList) ->
    io:format("~p - ~s ~n", [Num, DisplayName]),
    game_menu(GameListIter, Num+1, Alias,GameList).


gameRoom({GameModule, GameName, RoomPid}, ClientPid, Alias, 0) ->
    srv ! {enterGameRoom, Alias, ClientPid, {GameModule, RoomPid}},
    gameRoom({GameModule, GameName, RoomPid}, ClientPid, Alias, 1);
gameRoom({GameModule, GameName, RoomPid}, ClientPid, Alias,1) ->
    RoomPid ! {input, ClientPid, Alias, getInput()},
    gameRoom({GameModule, GameName, RoomPid}, ClientPid, Alias, 1).

printPlayers([]) -> 
	io:format("~n", []);
printPlayers([Player | PlayerList]) ->
    io:format("~s, ", [Player]),
    printPlayers(PlayerList).

quit(ClientPid) ->
    io:format("~nBye!~n",[]),
    srv ! {quit, self()},
    ClientPid ! {quit}. 

%% @doc a funtion that handles all the messages from the game room.
%% @hidden

receiver(GameList,Num,Alias) ->
    receive 
	{message, Sender, Message} ->
	    io:format("~s> ~s~n",[Sender, Message]),
	    receiver(GameList, Num, Alias);
	{back} -> 
	    ok;
	{challenge, GameRoom, {OriginPid, OriginAlias}} ->
		io:format("~s is challenging you! Do you accept? y/n~n", [OriginAlias]),
		case hd(getInput()) of
			$y ->
				GameRoom ! {initiateGame, [{OriginPid, OriginAlias}, {self(), Alias}]};
			_ ->
				io:format("Challenge declined.~n", []),
				OriginPid ! {declineChallenge, Alias}
		end,
		receiver(GameList, Num, Alias);

	{declineChallenge, OriginAlias} ->
		io:format("~s has declined your challenge.~n", [OriginAlias]),
		receiver(GameList, Num, OriginAlias);
	{game, GamePid} ->
		{game, GamePid};
	{printPlayers, PlayerList} ->
	    printPlayers(PlayerList),
	    receiver(GameList, Num, Alias)
    end.

%% @doc shows number of clients connected to the server.
%% @spec numConnected() -> {getNumCluents,self()}
%% @hidden
numConnected() ->
    srv ! {getNumClients, self()},
    receive
	{numClients, NumClients} ->
	    io:format("Number of clients connected: ~p~n", [NumClients])
    after 1000 ->
	    io:format("Failed to receive number of clients~n", [])
    end.
%% @doc gets input from user.
%% @spec getInput() -> trim(Input)
%% @hidden

getInput() ->
    Input = io:get_line(""),
    trim(Input).

%% @doc askes the user for an int.
%% @hidden
getNumber() ->
    case io_lib:fread("~d", getInput()) of
	{ok, Num, _} -> hd(Num);
	{error, _} -> {error}
    end.

%% @doc takes away "\n" from the string.
%% @hidden

trim(String) ->
    string:strip(string:strip(String, both, $\n)).
%% @doc sends {quit,self()} to the server.
%% @spec quit(ClientPid) -> {quit}
%% @hidden

%% @doc prints the help information.
%% @hidden

help(ClientPid,Alias) ->
    io:format("~n --Help-- ~n~n", []),
    io:format("SELECT GAME:~n",[]),
    io:format("Press 1 to get the list of available games.~n",[]),
    io:format("You will then be able to choose the game you want to play.~n~n",[]),
    io:format("STATISTICS:~n",[]),
    io:format("Press 2 to get the information about the current users online.~n~n",[]),
    io:format("QUIT:~n",[]),
    io:format("Press 4 to leave the server.~nYou will then return to the directory you where in before you connected to Earl's Game Club.~n~n",[]),
    
    io:format("Press [ENTER] to go back to Main Menu~n", []),
    getInput(),    
    main_menu(ClientPid,Alias).

gameMode(GamePid) ->
    receive
	{output, Message} ->
	    io:format("~s~n", [Message]),
		gameMode(GamePid);
	{input} ->
	    GamePid ! {input, getInput()},
		gameMode(GamePid);
	{inputNumber} ->
		GamePid ! {input, getNumber()},
		gameMode(GamePid);
	{finish} ->
	    ok
    end.
    

%% HELP FUNCTIONS %%

%% @hidden
runtest() ->
    test(),
    init:stop().

%% @hidden
trim_test() ->
    ?assertEqual("Test", trim("    Test   \n")).
