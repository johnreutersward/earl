
-module(gameAPI).
-export([init/2, getInput/1, print/2,getPlayer/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GameAPI functions 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Game, Players) ->
	State = Game:init(Players),
	run(Game, State, Players, []).

run(Game, State, Players, []) ->
	run(Game, State, Players, Players);
run(Game, State, Players, [NextPlayer | RemainingPlayers]) ->
	case Game:checkFinished(State, Players) of
		{true, Winner} ->
			finish(Winner, Players);
		{draw} ->
			draw(Players);
		{false} ->
			NewState = Game:nextTurn(State, NextPlayer,Players),
			run(Game, NewState, Players, RemainingPlayers)
	end.

finish({_, WinnerAlias}, Players) ->
	print("The winner is "++WinnerAlias++"! Congratulations!\n", Players).

draw(Players) ->
	print("The game is a draw. All are winners!\n",Players).

getInput(Pid) ->
	Pid ! {input},
	receive
		{input, Input} ->
			Input
	end.

print(_, []) ->
	ok;
print(Output, [Player | Players]) ->
	Player ! {output, Output},
	print(Output, Players).

getPlayer(Int,[]) ->
    [];
getPlayer(Int,Players) ->
    [lists:nth(Int,Players)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Functions to implement in Games
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init(Players) -> State

% checkFinished(State, Players) -> {true, Winner} | {draw} | {false}

% nextTurn(State, NextPlayer) -> State

