%% @author Simon Young, Tobias.
%% @doc This is the client handler with all of its functions.
%% The client Handler is the bridge between the clients and the server,
%% it realys all the data from the clients to the server.

-module(client_handler).
-export([handle_client/0,init/0]).

%% @doc starts the client handler as a bridge between the client and the server.

init() ->
    register(theServer,global:whereis_name(mainServer_PID)),
    io:format("Welcome!~n", []),
    create_alias().


create_alias() ->
    Alias = io:get_line("Define Username: "),
    theServer ! {checkAlias,self(),Alias},
    receive
	aliasTrue -> theServer ! {status,self(),Alias,[main]},
		     handle_client();
        aliasFalse -> io:format("Alias is already in use, please choose another Alias~n",[]),
		      create_alias()
    end.

%% @doc the client processes that handles all calls from the client and relays it to the server.

handle_client() ->
    receive		      
	{say,Msg} -> theServer ! {say,Msg,self()};
	{challange, User} -> theServer ! {challange, User, self()},
			     wait_challangeaccept(User); %% waits for challanger to accept or decline
	{challanged,User} -> io:format("~w has challenged you to a game, do you acceppt? (y/n)", [User]),
			    Answer = io:get_line(" "),
			    theServer ! {answer, Answer,self()},
			    gamemode();
	{quit} -> theServer ! {quit,self()},
		  ok
    end,
    handle_client().
	

%% HELP FUNCTIONS %%

wait_challangeaccept(User) ->
     receive
	 {answer, Answer} -> if
				 Answer == n -> io:format("~w Declines your challange~n", [User]);
				 Answer == y -> gamemode(); %% enters game mode
				 true -> handle_client()
					     
			     end
     after
	 60000 -> io:format("~w Declines your challange~n", [User])
     end.

gamemode() ->
    ok.
