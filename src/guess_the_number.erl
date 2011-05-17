-module(guess_the_number).
-export([init/1, nextTurn/2,checkFinished/2]).


init(Players) -> 
    Message = "Welcome to Guess The Number! You are supposed to guess a number between 0 and 100.~n",
    gameAPI:print(Message,Players),
    Number = random:uniform(),
    The_number = trunc(Number * 100),
    Current = {0,hej},
    {0,The_number,Current,Players}.

nextTurn({_,The_number,_,Players},{Next_player_Pid,Next_player_Alias}) ->
    gameAPI:print(Next_player_Alias++":s turn, guess the number: ",Players),
    The_guess = gameAPI:getInput(Next_player_Pid),
    case The_guess of
	The_number ->
	    gameAPI:print("That's the right number!!~n",Players),
	    {1,The_number,{Next_player_Pid, Next_player_Alias},Players};
	_ ->
	    if 
		The_guess < The_number ->
		    gameAPI:print("The number is bigger!~n",Players),
		    {0,The_number,{Next_player_Pid,Next_player_Alias},Players};
		true ->
		    gameAPI:print("The number is smaller!~n",Players),
		    {0,The_number,{Next_player_Pid,Next_player_Alias},Players}
	    end    
    end.



checkFinished({A,_,{_,C2},_},_) ->
    case A of
	1 ->
	    {true,C2};
	0 ->
	    {false}
    end.
		
