%% @author Tobias.Ericsson.0701@student.uu.se
%% @author Andreas.Hammar.5781@student.uu.se
%% @author Gabriella.Lundborg.6304@student.uu.se
%% @author Emma.Rangert.2142@student.uu.se
%% @author John.Reuterswärd.8971@student.uu.se
%% @author Simon.Young.0963@student.uu.se
%% @doc A game of tic tac toe using a game API desinged for Earl's Game Club.

-module(tictactoe).
-export([init/1, checkFinished/2, nextTurn/3]).
-import(gameAPI, [getInput/1,print/2,getPlayer/2]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Initiates a game of tic tac toe. It takes a list of players as a parameter
%% and sends a welcome message to each player. The function returns the initial state of the game.
%% A state is a quadruple with 4 lists in it. The first list has two tuples as elements and represents the
%% players and which one of them that starts. The other 3 lists represent the 3 diffrent rows of the game, each starting
%% with a, b or c.
%% @spec init(Players) -> State

init(Players) ->
    print("======== Tic Tac Toe ========\n", Players),
    print("Welcome to Tic Tac Toe, the very\nexciting game of wits and strategy\n",Players),
    print("let the Game Begin!\n",Players),
    PlayerList = [X || {X,_} <- Players],
    InitiatedPlayers = [{lists:nth(1,PlayerList),"X"},{lists:nth(2,PlayerList),"O"}],
    print("You are Player 1!\n",[getPlayer(1,Players)]),
    print("You are Player 2!\n",[getPlayer(2,Players)]),
    State = {InitiatedPlayers,[a," "," "," "],[b," "," "," "],[c," "," "," "]},
    printState(State,Players),
    State.

%% @doc This function checks the state of the game. It takes a state and a player list as parameters.
%% The function analysis the state and detemines wheter the game is a draw, if a player has won or if the game is not
%% finnished. The function returns a tuple based on what the result is.
%% @spec checkFinished(State,Players) -> {draw} | {false} | {true,Player}

checkFinished({_,[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},Players) ->   
    State = checkWinner({'_',[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},Players),
    if
	A1 /= " ",
	A2 /= " ",
	A3 /= " ",
	B1 /= " ",
	B2 /= " ",
	B3 /= " ",
	C1 /= " ",
	C2 /= " ",
	C3 /= " ",
	State == {false} ->
	    {draw};
	true ->
	    checkWinner({'_',[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},Players)
    end.


%% @doc This function takes as parameters the state of the game and player list.
%% The function checks the state and determines if a player has won. 
%% @spec checkWinner(State,Players) -> {false} | {true,Player}
%% @hidden

checkWinner({_,[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},Players) ->
    if
	A1 == A2, A1 == A3, A1 == "X" ->
	    {true,getPlayer(1,Players)};
	A1 == B2, A1 == C3, A1 == "X" ->
	    {true,getPlayer(1,Players)};
	A3 == B2, A3 == C1,A3 == "X" ->
	    {true,getPlayer(1,Players)};
	A1 == B1,A1 == C1,A1 == "X" ->
	    {true,getPlayer(1,Players)};
	B1 == B2,B1 == B3,B1 == "X" ->
	    {true,getPlayer(1,Players)};
	C1 == C2,C1 == C3,C1 == "X" ->
	    {true,getPlayer(1,Players)};
	A2 == B2,A2 == C2,A2 == "X" ->
	    {true,getPlayer(1,Players)};
	A3 == B3,A3 == C3,A3 == "X" ->
	    {true,getPlayer(1,Players)};
	A1 == A2,A1 == A3,A1 == "O" ->
	    {true,getPlayer(2,Players)};
	A1 == B2,A1 == C3,A1 == "O" ->
	    {true,getPlayer(2,Players)};
	A3 == B2,A3 == C1,A3 == "O" ->
	    {true,getPlayer(2,Players)};
	A1 == B1,A1 == C1,A1 == "O" ->
	    {true,getPlayer(2,Players)};
	B1 == B2,B1 == B3,B1 == "O" ->
	    {true,getPlayer(2,Players)};
	C1 == C2,C1 == C3,C1 == "O" ->
	    {true,getPlayer(2,Players)};
	A2 == B2,A2 == C2,A2 == "O" ->
	    {true,getPlayer(2,Players)};
	A3 == B3,A3 == C3,A3 == "O" ->
	    {true,getPlayer(2,Players)};
	true -> {false}
    end.


%% @doc This function handles the current turn. It takes as parameters the current state of the game,
%% the current player, and the list of players. The function prompts the current player for an input and
%% handles the input then prints out the new state for each player and then returns it.
%% @spec nextTurn(State,Player,Players) -> NewState

nextTurn({[{Player1,"X"},{Player2,"O"}],[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},{PlayerPid,Alias},Players) ->
    print("Your Turn!\n",[{PlayerPid,Alias}]),
    if 
	PlayerPid == Player1 ->
	    Move = "X";
	PlayerPid == Player2 ->
	    Move = "O"
    end,
    PlayerPid ! {input},
    receive
	{input,Input} ->
	    case Input of
		_ when Input /= "a1" ,
		       Input /= "a2" ,
		       Input /= "a3" ,
		       Input /= "b1" ,
		       Input /= "b2" ,
		       Input /= "b3" ,
		       Input /= "c1" ,
		       Input /= "c2" ,
		       Input /= "c3" ->
		    print(Input ++ " Is not a Move!\n",[{PlayerPid,Alias}]),
		    State = nextTurn({[{Player1,"X"},{Player2,"O"}],[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},{PlayerPid,Alias},Players);
		Input ->
		    State = place(Input,Move,{PlayerPid,Alias},{[{Player1,"X"},{Player2,"O"}],[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]})
	    end
    end,
    if
	State == illegalmove ->
	    print("Illegal Move!\n",[{PlayerPid,Alias}]),
	    nextTurn({[{Player1,"X"},{Player2,"O"}],[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},{PlayerPid,Alias},Players);
	true ->
	    printState(State,Players),
	    State
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Help functions            
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%% @doc This function takes 4 parameters, two strings, a player list and the state of the game.
%% Move represents the input from the current player and depending on the move, the function changes the state accordingly.
%% Input is what should be placed at the named place, i.e "X", "O". Player is the player who's turn it is, and State is the current
%% state of the game. The function returns the new State.
%% @spec place(Move,Input,Player,State) -> NewState
%% @hidden

place(Move,Input,Player,State) ->
    case Move of
	"a1" ->
	    input("a1",Input,State,Player);
	"a2" ->
	    input("a2",Input,State,Player);
	"a3" ->
	    input("a3",Input,State,Player);
	"b1" ->
	    input("b1",Input,State,Player);
	"b2" ->
	    input("b2",Input,State,Player);
	"b3" ->
	    input("b3",Input,State,Player);
	"c1" ->
	    input("c1",Input,State,Player);
	"c2" ->
	    input("c2",Input,State,Player);
	"c3" ->
	    input("c3",Input,State,Player)
    end.

%% @doc This function checks if the move made by the current player is legal or not. The function takes as parameters
%% two strings and the state of the game. The first string, Move, is the move the player wants to do. Input is the mark of the player,
%% i.e "X", "O". If the move was illegal the function returns the atom illegal, otherwise it returns the new state. 
%% @spec input(Move,Input,State,Players) -> illegal | NewState
%% @hidden

input(Move,Input,{[{Pid1,Alias1},{Pid2,Alias2}],[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},_) ->
    case Move of
	"a1" ->
	    if
		A1 == " " ->
		    {[{Pid1,Alias1},{Pid2,Alias2}],[a,Input,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]};
		true ->
		    illegalmove
	    end;
	"a2" ->
	    if 
		A2 == " " ->
		    {[{Pid1,Alias1},{Pid2,Alias2}],[a,A1,Input,A3],[b,B1,B2,B3],[c,C1,C2,C3]};
		true ->
		    illegalmove
	    end;
	"a3" ->
	    if 
		A3 == " " ->
		    {[{Pid1,Alias1},{Pid2,Alias2}],[a,A1,A2,Input],[b,B1,B2,B3],[c,C1,C2,C3]};
		true ->
		    illegalmove
	    end;
	"b1" ->
	    if 
		B1 == " " ->
		    {[{Pid1,Alias1},{Pid2,Alias2}],[a,A1,A2,A3],[b,Input,B2,B3],[c,C1,C2,C3]};
		true ->
		    illegalmove
	    end;
	"b2" ->
	    if 
		B2 == " " ->
		    {[{Pid1,Alias1},{Pid2,Alias2}],[a,A1,A2,A3],[b,B1,Input,B3],[c,C1,C2,C3]};
		true ->
		    illegalmove
	    end;
	"b3" ->
	    if 
		B3 == " " ->
		    {[{Pid1,Alias1},{Pid2,Alias2}],[a,A1,A2,A3],[b,B1,B2,Input],[c,C1,C2,C3]};
		true ->
		    illegalmove
	    end;
	"c1" ->
	    if 
		C1 == " " ->
		    {[{Pid1,Alias1},{Pid2,Alias2}],[a,A1,A2,A3],[b,B1,B2,B3],[c,Input,C2,C3]};
		true ->
		    illegalmove
	    end;
	"c2" ->
	    if 
		C2 == " " ->
		    {[{Pid1,Alias1},{Pid2,Alias2}],[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,Input,C3]};
		true ->
		    illegalmove
	    end;
	"c3" ->
	    if 
		C3 == " " ->
		    {[{Pid1,Alias1},{Pid2,Alias2}],[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,Input]};
		true ->
		    illegalmove
	    end
    end.


%% @doc this function prints out the current state to each player playing the game.
%% It takes two parameters, the state of the game and the playerlist.
%% @spec printState(State,Players) -> ok
%% @hidden

printState(illegalmove,_) ->
    ok;
printState({_,[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},Players) ->
    print("================================\n",Players),
    print("====1===2===3====\n",Players),
    print("a | " ++ A1 ++ " | " ++ A2 ++ " | " ++ A3 ++ " |\n",Players),
    print("b | " ++ B1 ++ " | " ++ B2 ++ " | " ++ B3 ++ " |\n",Players),
    print("c | " ++ C1 ++ " | " ++ C2 ++ " | " ++ C3 ++ " |\n",Players),
    print("================================\n",Players).
    

%%%%%%%%%%%%%%%%%%%%%%%
% Eunit Test Functions
%%%%%%%%%%%%%%%%%%%%%%%

init_test() ->
    ?assertEqual({[{self(),"X"},{self(),"O"}],[a," "," "," "],[b," "," "," "],[c," "," "," "]},init([{self(),"alias1"},{self(),"alias2"}])).


checkFinished_test_() ->
    [?_assertEqual({draw},checkFinished({[{self(),"X"},{self(),"O"}],[a,"X","O","X"],[b,"O","O","X"],[c,"X","X","O"]},[{self(),alias1},{self(),alias2}])),
     ?_assertEqual({false},checkFinished({[{self(),"X"},{self(),"O"}],[a," ","O","X"],[b," ","O","X"],[c,"X"," ","O"]},[{self(),alias1},{self(),alias2}])),
     ?_assertEqual({true, {self(),alias1}},checkFinished({[{self(),"X"},{self(),"O"}],[a,"X","X","X"],[b,"O"," "," "],[c," "," ","O"]},[{self(),alias1},{self(),alias2}]))].

place_test_() ->
    [?_assertEqual({[{self(),"X"},{self(),"O"}],[a,"X"," "," "],[b," "," "," "],[c," "," "," "]},
		   place("a1","X",[{self(),alias1}],{[{self(),"X"},{self(),"O"}],[a," "," "," "],[b," "," "," "],[c," "," "," "]})),
     ?_assertEqual(illegalmove,place("a1", "X",[{self(),alias1}],{[{self(),"X"},{pid2,"O"}],[a,"O"," "," "],[b," "," "," "],[c," "," "," "]}))].

print_test() ->
    ?assertEqual(ok,print("hej",[{self(),"alias1"}])).

runtest() ->
    io:format("Now testing Tic tac toe~n",[]),
    test(),
    init:stop().

