%% @author Tobias.Ericsson.0701@student.uu.se
%% @author Andreas.Hammar.5781@student.uu.se
%% @author Gabriella.Lundborg.6304@student.uu.se
%% @author Emma.Rangert.2142@student.uu.se
%% @author John.Reuterswärd.8971@student.uu.se
%% @author Simon.Young.0963@student.uu.se
%% @doc The Client module. This module allows a user to connect to Earl's Game Club
%% and speak to that server with client functions.

-module(client).
-export([connect/0,wait/0, quit/0, runtest/0]).
-export([init/0]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Starts connect.
%% @spec init() -> connect()
	     
init() ->
    erlang:set_cookie(node(), earl_game_club),
    io:format("~n---------------------------------------~n", []),
    io:format("------  Earl's Game Club client  ------~n", []),
    io:format("---------------------------------------~n", []),
    connect().

%% @doc connects to the server and spawns the init function in a given server.
%% @spec connect() -> wait()
connect() ->
    io:format("~nPlease enter the Earl server you wish to connect to: ~n", []),
    Input = io:get_line("> "),
    Temp = string:strip(Input, both, $\n),
    Server = list_to_atom(Temp),
    Answer = net_adm:ping(Server),
    if	
	Answer == pong -> 
	    spawn(Server,client_handler,init,[self()]),
	    wait();
	true -> 
	    io:format("~nERROR: The specified server could not be found.~n",[]),
	    connect()
    end.
%% @doc Waits for {quit} message and then runs quit()
%% @spec wait() -> quit()
%% @hidden

wait() ->
    receive
	{quit} ->
	    quit();
	{ping, PID} ->
		PID ! {pong},
		wait()
    end.

%% @doc kills the program.
%% @hidden
quit() ->
    init:stop().

%% TEST CASES %%
%% @hidden

runtest() ->
    test(),
    init:stop().
