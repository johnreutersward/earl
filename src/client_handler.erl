
-module(client_handler).
-export([handle_client/0, init/0, runtest/0]).
-include_lib("eunit/include/eunit.hrl").

init() ->
    %%register(theServer,global:whereis_name(mainServer_PID)),
    io:format("Welcome!~n", []),
    create_alias().

create_alias() ->
    Alias1 = io:get_line("Define Username: "),
	Alias = string:strip(Alias1, both, $\n),
    theServer ! {checkAlias,self(),Alias},
    receive
		aliasTrue -> 
			theServer ! {setStatus, self(), Alias, [main]},
		    handle_client();
        aliasFalse -> 
			io:format("Alias is already in use, please choose another Alias~n", []),
		    create_alias()
    end.

handle_client() ->
    receive		      
		{say,Msg} -> 
			theServer ! {say, Msg, self()},
		    handle_client();
		{challange, User} -> 
			theServer ! {challange, User, self()},
			wait_challangeaccept(User), %% waits for challanger to accept or decline
			handle_client();
		{challanged,User} -> 
			io:format("~w has challenged you to a game, do you acceppt? (y/n)", [User]),
			Answer = io:get_line(" "),
			theServer ! {answer, Answer, self()},
			gamemode();
		{quit} -> 
			theServer ! {quit, self()},
			ok
    end.
	
%% HELP FUNCTIONS %%

wait_challangeaccept(User) ->
     receive
		{answer, Answer} -> 
			if
				 Answer == n -> 
					 io:format("~w Declines your challange~n", [User]);
				 Answer == y -> 
					 gamemode(); %% enters game mode
				 true -> 
					 handle_client()    
			end
     after 60000 -> 
		io:format("~w Declines your challange~n", [User])
     end.

gamemode() ->
    ok.

% Test cases

runtest() ->
	test(),
	init:stop().

