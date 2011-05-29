%% @author Tobias.Ericsson.0701@student.uu.se
%% @author Andreas.Hammar.5781@student.uu.se
%% @author Gabriella.Lundborg.6304@student.uu.se
%% @author Emma.Rangert.2142@student.uu.se
%% @author John.Reuterswärd.8971@student.uu.se
%% @author Simon.Young.0963@student.uu.se
%% @doc A game of guess the number using a game API desinged for Earl's Game Club.

-module(guess_the_number).
-export([init/1, nextTurn/3, checkFinished/2, runtest/0]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Initiates a game of guess the number. It takes a list of players as a parameter
%% and sends a welcome message to each player. It also randomly picks a number between 0 and 100.
%% The function returns the initial state of the game.
%% A state is a tuple of three elements. The first element is the state of the game, it can be gameInProgress or gameOver. 
%% The second element in the tuple represents the number that the players are supposed to find out.
%% The last element in  the tuple is a tuple representing the current player's pid and alias.
%% @spec init(Players) -> State

init(Players) -> 
    random:seed(erlang:now()),
    The_number = random:uniform(100),
    Message = "\nWelcome to Guess The Number! Guess a number between 0 and 100.\n",
    gameAPI:print(Message,Players),
	{gameInProgress, The_number, {noPid, noPlayer}}.

%% @doc This function handles the current turn. It takes as parameters the current state of the game,
%% the current player, and the list of players. The function informs which players turn it is and then prompts the current player for an input.
%% It informs the other players what the current player guessed. If the guess was correct the game state is changed to gameOver.
%% If the guess was worng, the function informs the players if the correct number is smaller or larger. 
%% Then the function returns the new state.
%% @spec nextTurn(State,Player,Players) -> NewState

nextTurn({_,The_number,_}, {PlayerPid, PlayerAlias}, Players) ->
    Remaining = lists:keydelete(PlayerPid, 1, Players),
	gameAPI:print("Your turn!", [{PlayerPid, PlayerAlias}]),
	gameAPI:print(PlayerAlias ++ "'s turn", Remaining),
    gameAPI:print("Guess the number: ", [{PlayerPid, PlayerAlias}]),
    The_guess = (gameAPI:getNumber(PlayerPid)),
	if
		is_pid(The_guess) ->
			gameAPI:print(element(2,lists:keyfind(The_guess, 1, Players)) ++ " has left the game", Players),
			{gameOver, The_number, hd(Remaining)};
		The_guess == The_number ->
			gameAPI:print(PlayerAlias ++ " guessed " ++ integer_to_list(The_guess), Remaining),
			gameAPI:print("That's the right number!!\n", Players),
			{gameOver, The_number, {PlayerPid, PlayerAlias}};
		The_guess < The_number ->
			gameAPI:print(PlayerAlias ++ " guessed " ++ integer_to_list(The_guess), Remaining),
			gameAPI:print("The number is bigger!\n", Players),
			{gameInProgress, The_number, {PlayerPid, PlayerAlias}};
		true ->
			gameAPI:print(PlayerAlias ++ " guessed " ++ integer_to_list(The_guess), Remaining),
			gameAPI:print("The number is smaller!\n", Players),
			{gameInProgress, The_number, {PlayerPid, PlayerAlias}}   
    end.

%% @doc This function checks the state of the game. It takes a state and a player list as parameters.
%% The function analysis the state and detemines wheter a player has won or if the game is not
%% finnished. The function returns a tuple based on what the result is.
%% @spec checkFinished(State,Players) -> {false} | {true,Player}

checkFinished({GameState,_, Player}, _) ->
    case GameState of
	gameOver ->
	    {true, Player};
	gameInProgress ->
	    {false}
    end.
		


%%%%%%%%%%%%%%%%%%%%%%%
% Eunit Test Functions
%%%%%%%%%%%%%%%%%%%%%%%

checkFinished_test_() ->
    [?_assertEqual({false},checkFinished({gameInProgress,40,{self(),alias1}},[{self(),alias1},{self(),alias2}])),
     ?_assertEqual({true, {self(),alias1}},checkFinished({gameOver,40,{self(),alias1}},[{self(),alias1},{self(),alias2}]))].

runtest() ->
    io:format("Now testing Guess the number~n",[]),
    test(),
    init:stop().
