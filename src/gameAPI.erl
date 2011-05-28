
-module(gameAPI).
-export([init/3, getInput/1, getNumber/1, print/2,getPlayer/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GameAPI functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Starts a game instance for the players in the Playerlist.
%% @spec init(Game, Playerlist) -> ok

init(Game, Players, GameRoomPid) ->
	lists:map( fun(X) -> erlang:monitor(process, element(1,X)) end, Players),
	State = Game:init(Players),
	run(Game, State, Players, [], GameRoomPid).

%% @doc Checks if the game is over and sends that information to all players OR runs a new state of the game.
%% @spec run(Game, State, Playerlist, Playerlist) -> ok

run(Game, State, Players, [], GameRoomPid) ->
	run(Game, State, Players, Players, GameRoomPid);
run(Game, State, Players, [NextPlayer | RemainingPlayers], GameRoomPid) ->
	case Game:checkFinished(State, Players) of
		{true, Winner} ->
			finish(Winner, Players, Game, GameRoomPid);
		{draw} ->
			draw(Players);
		{false} ->
			NewState = Game:nextTurn(State, NextPlayer, Players),
			run(Game, NewState, Players, RemainingPlayers, GameRoomPid)
	end.

%% @doc Sends a "win message" to all players and that the game has ended.
%% @spec finish(Player, Playerlist) -> ok

finish({_, WinnerAlias}, Players, Game, GameRoomPid) ->
	print("The winner is "++WinnerAlias++"!\n", Players),
	GameRoomPid ! {finish, WinnerAlias},
	send({finish}, Players).

%% @doc Sends a "draw message" to all players in the Playerlist.
%% @spec draw(Playerlist) -> ok

draw(Players) ->
	print("The game is a draw. Computer wins 0100100001000001!\n",Players),
	send({finish}, Players).


%% @doc Asks a player for an input. If that player has died that players Pid is returned.
%% @spec getInput(Pid) -> Input

getInput(Pid) ->
	Pid ! {input},
	receive
		{input, Input} ->
			Input;
		{'DOWN', _Reference, process, Pid, _Reason} ->
			Pid
	end.

%% @doc Sends a request to the specified Pid for a numeric input. The function returns either the read input or an error if an invalid character was specified. If that player has died that players Pid is returned.
%% @spec getNumber(Pid) -> Integer | {error} 

getNumber(Pid) ->
	Pid ! {inputNumber},
	receive
		{input, Input} ->
			Input;
		{'DOWN', _Reference, process, Pid, _Reason} ->
			Pid
	end.

%% @doc Sends a message to all players in list.
%% @spec send(Output, Playerslist) -> ok

send(_, []) ->
	ok;
send(Output, [{Pid, _} | Players]) ->
	Pid ! Output,
	send(Output, Players).

%% @doc Sends a message (to be printed) to all players in list. 
%% @spec print(Output, Players) -> ok

print(_, []) ->
	ok;
print(Output, [{Pid,Alias} | Players]) ->
	Pid ! {output, Output},
	print(Output, Players).

%% @doc Gets the nth player from the list.
%% @spec init() -> ok
%% @spec getPlayer(Int, Playerlist) -> Player

getPlayer(Int,[]) ->
    ok;
getPlayer(Int,Players) ->
    lists:nth(Int,Players).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Functions to implement in Games
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init(Players) -> State

% checkFinished(State, Players) -> {true, Winner} | {draw} | {false}

% nextTurn(State, NextPlayer, Players) -> State

