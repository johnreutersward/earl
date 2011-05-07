%% @author Tobias Ericsson <tobiasericsson90@hotmail.com>
%% @author Andreas Hammar <andreashammar@gmail.com>
%% @author John Reuterswärd <rojters@gmail.com>
%% @author Simon Young <youngen.simon@gmail.com>
%% @doc This is the main server that handles all the communication between
%% clients and the database.

-module(server).
-export([init/0,server/0,runtest/0,msgSender/3]).
-include_lib("eunit/include/eunit.hrl").

init() ->
    register(srv, self()),
    register(db,spawn(database,init,[])),
    io:format("-----------------------------------------------~n", []),
    io:format("------  Earl Game Club server initiated  ------~n", []),
    io:format("-----------------------------------------------~n", []),
    io:format("         Host:   ~s~n", [net_adm:localhost()]),
    io:format("         Node:   ~s~n", [node()]),
    io:format("         Cookie: ~s~n", [erlang:get_cookie()]),
    io:format("-----------------------------------------------~n~n", []),
    server().

server() ->
    receive
	{say, Msg, Pid, Alias} ->
	    io:format("Server: Received 'say', spawning message-sender~n", []),
	    spawn(server, msgSender, [Msg, Pid, Alias]);
	{setStatus, Pid, Alias,Status} ->  
	    io:format("Server: Received 'setStatus' request, forwarding to database~n", []),
	    db ! {setStatus, Pid, Alias, Status};
	{checkAlias, Alias, Origin} -> 
	    io:format("Server: Received 'checkAlias', forwarding to db~n", []),
	    db ! {checkAlias, Alias, Origin};	       	      
	{quit, Pid} ->
	    io:format("Server: Received 'quit' from pid, sending removal request to db~n", []),
	    db ! {remove,Pid};
	{debug, Msg} ->
	    io:format("~s~n", [Msg]);
	{getNumClients,Origin} ->
	    io:format("server recevied 'getNumClients'",[]),
	    db ! {getNumClients,Origin}
	    
    end,
    server().

%% HELP FUNCTIONS %%


%% @doc Gets the Status of the process with Process Id Pid

getStatus(Pid) ->
    db ! {getStatus,Pid,self()},
    receive
	{answer,Status} ->
	    Status
    end.

%% @doc gets a list of all the processes with the status Status.

getStatusList(Status) ->
    db ! {getStatusList, Status, self()},
    receive
	{answer, StatusList} ->
	    StatusList
    end.

%% @doc sends a message to all the PIDs in the List

sendMsg([], _, _) -> ok;
sendMsg([H | T], Alias, Msg) ->
    H ! {message, Alias,Msg},
    sendMsg(T, Alias, Msg).

msgSender(Msg, Pid, Alias) ->
    Status = getStatus(Pid),
    StatusList = getStatusList(Status),
    sendMsg(StatusList, Alias, Msg).

% Test cases

runtest() ->
    test(),
    init:stop().

