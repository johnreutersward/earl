
-module(gameAPI).
-export([init/2, getInput/1, print/2]).

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
		{false} ->
			NewState = Game:nextTurn(State, NextPlayer),
			run(Game, NewState, Players, RemainingPlayers)
	end.

finish({_, WinnerAlias, _}, Players) ->
	print("The winner is "++WinnerAlias++"! Congratulations!\n", Players).


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Functions to implement in Games
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% init() -> State

% checkFinished(State) -> Winner

% nextTurn(NextPlayer) -> State

