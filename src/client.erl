%% @author Simon Young <youngen.simon@gmail.com>
%% @doc The Client module. This module allows a user to connect to Earl's Game Club
%% and speak to that server with client functions.
%% @todo add more client functions. Complete gamemode function.

-module(client).
-export([connect/0,say/2,challange/2,quit/1, runtest/0]).
-export([init/0]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Starts the client.
%% @spec start() -> client
	     
init() ->
    connect().

client(ClientHandler) ->
    Command = io:get_line("> "),
    Command2 = string:strip(Command, both, $\n), %% tar bort \n
    command(string:tokens(Command2, " "),ClientHandler),
    client(ClientHandler).

%% @doc connects to the server
%% @spec connect() -> {connect,Server}

connect() ->
    Input = io:get_line("Connect to: "),
    Temp = string:strip(Input, both, $\n),
    Server = list_to_atom(Temp),
    Answer = net_adm:ping(Server),
    if	
	Answer == pong -> ClientHandler = spawn(Server,client_handler,init,[]),
			  client(ClientHandler);	 
	true -> io:format("Connection ERROR~n",[]),
		connect()
    end.

%% @doc this function parses String into difrent commands.

command([Command | Input],ClientHandler) ->
    case(Command) of
	"say" ->
	    say(Input,ClientHandler);
	"challange" ->
	    challange(Input,ClientHandler);
	"quit" -> 
	    quit(ClientHandler);
	true -> io:format("Unknown command~n",[])
    end.

%% @doc sends a message to the server
%% @spec say() -> {say,Msg}

say(Msg,ClientHandler) ->
    ClientHandler ! {say,Msg}.

%% @doc sends a challange to another user
%% @spec challange() -> {challange,User}

challange(User,ClientHandler) ->
    ClientHandler ! {challange,User}. 

%% @doc log off the server.
%% @spec quit() -> {quit}

quit(ClientHandler) ->
    ClientHandler ! {quit},
    init:stop().


%% TEST CASES %%

runtest() ->
    test(),
    init:stop().
