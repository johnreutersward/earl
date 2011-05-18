%% @author Tobias Ericsson <tobiasericsson90@hotmail.com>
%% @author Andreas Hammar <andreashammar@gmail.com>
%% @author Gabriella Lundborg <gabriella_lundborg@hotmail.com>
%% @author Emma Rangert <emma.rangert@gmail.com>
%% @author John Reuterswärd <rojters@gmail.com>
%% @author Simon Young <youngen.simon@gmail.com>
%% @doc A game tic tac toe using a game API desinged for earls game club.

-module(tictactoe).
-export([init/1, checkFinished/2, nextTurn/3]).
-import(gameAPI, [getInput/1,print/2,getPlayer/2]).
-include_lib("eunit/include/eunit.hrl").

%% @doc Initiates a game of tic tac toe
%% @spec init(Players) -> State

init(Players) ->
    print("======== Tic Tac Toe ========\n", Players),
    print("Welcome to Tic Tac Toe, the very\nexciting game of wits and strategy\n",Players),
    print("let the Game Begin!\n",Players),
    PlayerList = [X || {X,_} <- Players],
    InitiatedPlayers = [{lists:nth(1,PlayerList),"X"},{lists:nth(2,PlayerList),"O"}],
    print("You are Player 1!\n",getPlayer(1,Players)),
    print("You are Player 2!\n",getPlayer(2,Players)),
    State = {InitiatedPlayers,[a," "," "," "],[b," "," "," "],[c," "," "," "]},
    printState(State,Players),
    State.

%% @doc checks if someone has won or if a draw occurs.
%% @spec checkFinished(State,Players) -> {draw}| {false}| {true,Player}

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


%% @doc checks the state if someone has won
%% @spec checkWinner(State,Players) -> {false}| {true,Player}

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


%% @doc handles the next turn, enables the current player to make a move
%% @spec nextTurn(State,Player,Players) -> ok

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
		    State = place(Input,Move,{PlayerPid,Alias},{[{Player1,"X"},{Player2,"O"}],[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},Players)
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


%% @doc A function that handles the moves a player can do
%% @spec place(Move,Input,Player,State, Players) -> State
%% @hidden

place(Move,Input,Player,State,_) ->
    case Move of
	"a1" ->
	    input("a1",Input,State,Player);
	"a2" ->
	    input("a1",Input,State,Player);
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


%% @doc prints out the current state of the game
%% @hidden

printState(illegalmove,_) ->
    ok;
printState({_,[a,A1,A2,A3],[b,B1,B2,B3],[c,C1,C2,C3]},Players) ->
    print("================================\n",Players),
    print("====1===2===3====\n",Players),
    print("A % " ++ A1 ++ " % " ++ A2 ++ " % \n" ++ A3,Players),
    print("B % " ++ B1 ++ " % " ++ B2 ++ " % \n" ++ B3,Players),
    print("C % " ++ C1 ++ " % " ++ C2 ++ " % \n" ++ C3,Players),
    print("================================\n",Players).
    

%%%%%%%%%%%%%%%%%%%%%%%
% Eunit Test Functions
%%%%%%%%%%%%%%%%%%%%%%%



init_test() ->
    ?assertEqual({[{pid1,"X"},{pid2,"O"}],[a," "," "," "],[b," "," "," "],[c," "," "," "]},init([{pid1,"alias1"},{pid2,"alias2"}])).


checkFinished_test_() ->
    [?_assertEqual({draw},checkFinished({[{pid1,"X"},{pid2,"O"}],[a,"X","O","X"],[b,"O","O","X"],[c,"X","X","O"]},[{pid1,alias1},{pid2,alias2}])),
     ?_assertEqual({false},checkFinished({[{pid1,"X"},{pid2,"O"}],[a," ","O","X"],[b," ","O","X"],[c,"X"," ","O"]},[{pid1,alias1},{pid2,alias2}])),
     ?_assertEqual({true, [{pid1,alias1}]},checkFinished({[{pid1,"X"},{pid2,"O"}],[a,"X","X","X"],[b,"O"," "," "],[c," "," ","O"]},[{pid1,alias1},{pid2,alias2}]))].

place_test_() ->
    [?_assertEqual({[{pid1,"X"},{pid2,"O"}],[a,"X"," "," "],[b," "," "," "],[c," "," "," "]},
		   place("a1","X",[{pid1,alias1}],{[{pid1,"X"},{pid2,"O"}],[a," "," "," "],[b," "," "," "],[c," "," "," "]},[{pid1,alias1},{pid2,alias2}])),
     ?_assertEqual(illegalmove,place("a1", "X",[{pid1,alias1}],{[{pid1,"X"},{pid2,"O"}],[a,"O"," "," "],[b," "," "," "],[c," "," "," "]},[{pid1,alias1},{pid2,alias2}]))].

print_test() ->
    ?assertEqual(ok,print("hej",[{pid1,"alias1"}])).
