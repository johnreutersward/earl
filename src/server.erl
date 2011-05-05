-module(server).
-export([init/0,server/0,checkAlias/2, runtest/0]).
-include_lib("eunit/include/eunit.hrl").

init() ->
    register(db,spawn(database,init,[])),
    register(theServer, spawn(server, server, [])).

server() ->
    io:format("Server pid: ~w~n", [self()]),
    receive
	{setStatus, Pid, Alias, Status} ->  
	    db ! {client,setStatus,Pid,Alias,Status};
	{checkAlias, Pid, Alias} -> 
	    db ! {checkAlias,Pid,Alias};	       	      
	{quit,Pid} ->
	    db ! {remove,Pid}	
    end,
    server().

checkAlias(Pid, Alias) ->
    Answer = ets:match(clientTable, {'$1', Alias, '_'}),
    if
	Answer == [] -> 
	    Pid ! aliasTrue;
	true -> 
	    Pid ! aliasFalse
    end.

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

