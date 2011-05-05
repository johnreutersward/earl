%% @author Simon Young <youngen.simon@gmail.com>
%% @doc The Client module. This module allows a user to connect to Earl's Game Club
%% and speak to that server with client functions.
%% @todo add more client functions. Complete gamemode function.

-module(client).
-export([connect/1,say/1,challange/1,quit/0, runtest/0]).
-export([start/0]).

-include_lib("eunit/include/eunit.hrl").

client() ->
    Command = io:get_line("> "),
    Command2 = string:strip(Command, both, $\n),
    command(string:tokens(Command2, " ")).

%% @doc this function parses String into difrent commands.

command([Command | Input]) ->
    case(Command) of
	"connect" ->
	    connect(list_to_atom(hd(Input)));
	"say" ->
	    say(Input);
	"challange" ->
	    challange(Input);
	"quit" -> 
	    quit();
	true -> io:format("Unknown command~n",[])
    end,
    client().

%% @doc Starts the client.
%% @spec start() -> client
	     
init() ->
    client().

%% @doc connects to the server
%% @spec connect() -> {connect,Server}

connect(Server) ->
    Answer = net_adm:ping(Server),
    if
	
	Answer == pong -> global:whereis_name(mainServer_PID),
			  spawn(Server,client_handler,init,[]),
			  %register(clientHandler, spawn(Server,client_handler,init,[])),
			 client();
	true -> io:format("Connection ERROR~n",[]),
		client()
    end.

%% @doc sends a message to the server
%% @spec say() -> {say,Msg}

say(Msg) ->
    clientHandler ! {say,Msg},
    client().

%% @doc sends a challange to another user
%% @spec challange() -> {challange,User}
	     
challange(User) ->
    clientHandler ! {challange,User},
    client().

%% @doc log off the server.
%% @spec quit() -> {quit}
	     
quit() ->
    clientHandler ! {quit},
    client().


%% TEST CASES %%

runtest() ->
	test(),
	init:stop().

