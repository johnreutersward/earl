-module(server).
-export([init/0,server/0,runtest/0,msgSender/3]).
-include_lib("eunit/include/eunit.hrl").

init() ->
    register(db,spawn(database,init,[])),
    register(theServer, self()),
    server().

server() ->
    io:format("Server pid: ~w~n", [self()]),
    receive
	{say,Msg,Pid,Alias} ->
	    spawn(server,msgSender,[Msg,Pid,Alias]);
	{setStatus, Pid, Alias,Status} ->  
	    db ! {client,setStatus,Pid,Alias,Status};
	{checkAlias, Pid, Alias} -> 
	    db ! {checkAlias,Pid,Alias};	       	      
	{quit,Pid} ->
	    db ! {remove,Pid};
	{debug, Msg} ->
		io:format("~s~n", Msg)	
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
    db ! {getStatusList,Status,self()},
    receive
	{answer,StatusList} ->
	    StatusList
    end.
	    
%% @doc sends a message to all the PIDs in the List

sendMsg([],_,_) -> ok;
sendMsg([H | T],Alias,Msg) ->
    H ! {message, Alias,Msg},
    sendMsg(T,Alias,Msg).

msgSender(Msg,Pid,Alias) ->
    Status = getStatus(Pid),
    StatusList = getStatusList(Status),
    sendMsg(StatusList,Alias,Msg).

% Test cases

runtest() ->
    test(),
    init:stop().

