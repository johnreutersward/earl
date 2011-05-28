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
    gameAPI:print(Next_player_Alias++":s turn\n",Players),
    gameAPI:print("guess the number: ",[{Next_player_Pid,Next_player_Alias}]),
    The_guess = (gameAPI:getNumber(Next_player_Pid)),
    Remaining= lists:keydelete(Next_player_Pid,1,Players),
	if 
		is_number(The_guess) ->		
			gameAPI:print(Next_player_Alias++" guessed "++integer_to_list(The_guess)++"\n",Remaining);
		true ->
			ok
	end,
	if
		is_pid(The_guess) ->
			gameAPI:print(element(2,lists:keyfind(The_guess, 1, Players)) ++ " has left the game", Players),
			{1, The_number, hd(Remaining)};
		The_guess == The_number ->
			gameAPI:print("That's the right number!!\n", Players),
			{1, The_number, {Next_player_Pid, Next_player_Alias}};
		The_guess < The_number ->
			gameAPI:print("The number is bigger!\n",Players),
			{0,The_number,{Next_player_Pid,Next_player_Alias}};
		true ->
			gameAPI:print("The number is smaller!\n",Players),
			{0,The_number,{Next_player_Pid,Next_player_Alias}}   
    end.



checkFinished({A,_,C},_) ->
    case A of
	1 ->
	    {true,C};
	0 ->
	    {false}
    end.
		
