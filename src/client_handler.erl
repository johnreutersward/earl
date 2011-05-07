
-module(client_handler).
-export([init/1, create_alias/1, main_menu/1, game_menu/0, getInput/0, trim/1, runtest/0]).
-include_lib("eunit/include/eunit.hrl").

init(ClientPid) ->
    NoOnline = 2,
	io:format("~n-- Welcome to Earl's Game Club!~nNumber of users online: ~w~n~n", [NoOnline]),
    create_alias(ClientPid).

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

getInput() ->
	Input = io:get_line(""),
	trim(Input).

trim(String) ->
	string:strip(string:strip(String, both, $\n)).

quit(ClientPid) ->
    io:format("~nBye!~n",[]),
    srv ! {quit, self()},
	ClientPid ! {quit}. 


%% HELP FUNCTIONS %%

% wait_challangeaccept(User,Alias,Status) ->
%   receive
%	{answer, Answer} -> 
%	    if
%		Answer == n -> 
%		    io:format("~w Declines your challange~n", [User]);
%		Answer == y -> 
%		    gamemode() %% enters game mode
%	    end
%    after 60000 -> 
%	    io:format("~w Declines your challange~n", [User])
%    end.

%gamemode() ->
%    ok.

% Test cases

runtest() ->
    test(),
    init:stop().
trim_test() ->
	?assertEqual("Test", trim("    Test   \n")).
