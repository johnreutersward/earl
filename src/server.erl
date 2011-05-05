-module(server).
-export([init/0,server/0,runtest/0]).
-include_lib("eunit/include/eunit.hrl").

init() ->
    register(db,spawn(database,init,[])),
    register(theServer, spawn(server, server, [])).

server() ->
    io:format("Server pid: ~w~n", [self()]),

% works in init, but fails in server
%------Debug------------------------
%	ets:match(clientTable, {'$1', "foo", '_'}),
%	ets:insert(clientTable, {self(), "youngen", [main]}),
%-----------------------------------
	receive
	    {setStatus, Pid, Alias, Status} ->  
		db ! {client,setStatus,Pid,Alias,Status};
	    {checkAlias, Pid, Alias} -> 
		db ! {checkAlias,Pid,Alias};	       	      
	    {quit,Pid} ->
		db ! {remove,Pid}
    end,
    server().
% Test cases

runtest() ->
	test(),
	init:stop().
checkAlias_test() ->
	ets:new(clientTable, [set, public, named_table]),
	checkAlias(self(), "foo"),
	ets:insert(clientTable, {self(), "foo", [main]}),
	checkAlias(self(), "foo"),
	checkAlias(self(), "bar").

