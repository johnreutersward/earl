-module(guess_the_number).
-export([init/1, nextTurn/3,checkFinished/2]).


init(Players) -> 
    random:seed(erlang:now()),
    The_number = random:uniform(100),
    Message = "Welcome to Guess The Number! You are supposed to guess a number between 0 and 100.\n",
    gameAPI:print(Message,Players),
    Current = {0,hej},
    {0,The_number,Current}.

nextTurn({_,The_number,_},{Next_player_Pid,Next_player_Alias},Players) ->
    gameAPI:print(Next_player_Alias++":s turn, guess the number: ",Players),
    The_guess = (gameAPI:getNumber(Next_player_Pid)),
    gameAPI:print(Next_player_Alias++" guessed "++integer_to_list(The_guess)++"\n",Players),
    case The_guess of
	The_number ->
	    gameAPI:print("That's the right number!!\n",Players),
	    {1,The_number,{Next_player_Pid, Next_player_Alias}};
	_ ->
	    if 
		The_guess < The_number ->
		    gameAPI:print("The number is bigger!\n",Players),
		    {0,The_number,{Next_player_Pid,Next_player_Alias}};
		true ->
		    gameAPI:print("The number is smaller!\n",Players),
		    {0,The_number,{Next_player_Pid,Next_player_Alias}}
	    end    
    end.



checkFinished({A,_,C},_) ->
    case A of
	1 ->
	    {true,C};
	0 ->
	    {false}
    end.
		
