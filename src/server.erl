-module(server).
-export([init/0,server/0,runtest/0]).
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
%% <<<<<<< HEAD

checkAlias(Pid, Alias) ->
    Answer = ets:match(clientTable, {'$1', Alias, '_'}),
    if
	Answer == [] -> 
	    Pid ! aliasTrue;
	true -> 
	    Pid ! aliasFalse
    end.

%% =======
%% >>>>>>> 30d7f39f95bc91f38ebc00607e76614c10442b68
%% Test cases

runtest() ->
    test(),
    init:stop().
checkAlias_test() ->
    ets:new(clientTable, [set, public, named_table]),
    checkAlias(self(), "foo"),
    ets:insert(clientTable, {self(), "foo", [main]}),
    checkAlias(self(), "foo"),
    checkAlias(self(), "bar").

