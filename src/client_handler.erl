%% @author Tobias Ericsson <tobiasericsson90@hotmail.com>
%% @author Andreas Hammar <andreashammar@gmail.com>
%% @author John Reuterswärd <rojters@gmail.com>
%% @author Simon Young <youngen.simon@gmail.com>
%% @doc This module has all the functions that the client needs on the server side.
%% This functions gives the user all the information and ability to interact with other users
%% and the server.

-module(client_handler).
-export([init/1, create_alias/1, main_menu/1, game_menu/0, getInput/0, trim/1, runtest/0, numConnected/0]).
-include_lib("eunit/include/eunit.hrl").

%% @doc initiates the client handler.
%% @spec init(ClientPid) -> create_alias(ClientPid)

init(ClientPid) ->
    io:format("~n-- Welcome to Earl's Game Club!~n", []),
    numConnected(),
    create_alias(ClientPid).

%% @doc creates a user, when the client comes online.
%% @spec create_alias(ClientPid) -> main_menu(ClientPid)

create_alias(ClientPid) ->
    io:format("Input a Username: ", []),
    Alias = getInput(),
    srv ! {checkAlias, Alias, self()},
    io:format("Handler: Waiting for server confirmation~n", []),
    receive
	aliasValid -> 
	    srv ! {setStatus, self(), Alias, [main]},
	    main_menu(ClientPid);
        aliasInvalid -> 
	    io:format("Alias is already in use, please choose another Alias~n", []),
	    create_alias(ClientPid)
    end.

%% @doc this is the men menu that the user sees upon entering the server.
%% from this function the user can enter game_menu() or quit().

main_menu(ClientPid) ->
    io:format("~n --Main Menu-- ~n", []),
    io:format("1 - Select game ~n", []),
    io:format("2 - Show statistics ~n", []),
    io:format("3 - Help ~n", []),
    io:format("4 - Quit ~n", []),
    
    case(getInput()) of
	"1" ->
	    game_menu(),
	    main_menu(ClientPid);
	"2" ->
	    io:format("~nStats are not yet implemented.~n", []),
	    main_menu(ClientPid);
	"3" ->
	    io:format("~nHelp is not yet implemented.~n", []),
	    main_menu(ClientPid);
	"4" ->
	    quit(ClientPid);
	_ ->
	    io:format("~nIllegal command~n",[]),
	    main_menu(ClientPid)
    end.

%% @doc in this menu the user can choose witch game to join.

game_menu() ->
    io:format("~n --Game Menu--~n", []),
    io:format("1 - Tic tac toe ~n", []),
    io:format("2 - Guess a number ~n", []),
    io:format("3 - Back to Main Menu~n", []),
    case(getInput()) of
	"1" ->
	    io:format("'Tic tac toe' is not implemented yet.~n", []),
	    game_menu();
	"2" ->
	    io:format("'Guess a number' is not implemented yet.~n", []),
	    game_menu();
	"3" ->
	    ok;
	_ ->
	    io:format("~nIllegal command~n", []),
	    game_menu()
    end.
%% @doc shows number of clients connected to the server.
%% @spec numConnected() -> {getNumCluents,self()}

numConnected() ->
    srv ! {getNumClients, self()},
    receive
	NumClients ->
	    io:format("Number of clients connected: ~p~n", [NumClients])
    after 1000 ->
	    io:format("Failed to receive number of clients~n", [])
    end.
%% @doc gets input from user.
%% @spec getInput() -> trim(Input).

getInput() ->
    Input = io:get_line(""),
    trim(Input).
%% @doc takes away "\n" from the string.

trim(String) ->
    string:strip(string:strip(String, both, $\n)).
%% @doc sends {quit,self()} to the server.
%% @spec quit(ClientPid) -> {quit}

quit(ClientPid) ->
    io:format("~nBye!~n",[]),
    srv ! {quit, self()},
    ClientPid ! {quit}. 


%% HELP FUNCTIONS %%

runtest() ->
    test(),
    init:stop().
trim_test() ->
	?assertEqual("Test", trim("    Test   \n")).
