-module(server).
-export([init/0,server/0,checkAlias/2, runtest/0]).
-include_lib("eunit/include/eunit.hrl").

init() ->
    ets:new(clientTable, [set, public, named_table]),
%--------Debug---------------------
%	ets:insert(ClientTable, {self(), "foo", [main]}),
%	io:format("ETS: ~w~n", [ets:info(clientTable)]),
%----------------------------------
	global:register_name(mainServer_PID, spawn(server, server, [])).

server() ->
    io:format("Server pid: ~w~n", [self()]),

% works in init, but fails in server
%------Debug------------------------
%	ets:match(clientTable, {'$1', "foo", '_'}),
%	ets:insert(clientTable, {self(), "youngen", [main]}),
%-----------------------------------
	receive
		{status, Pid, Alias, Status} ->  
			ets:insert(clientTable, {Pid, Alias, Status});
		{checkAlias, Pid, Alias} -> 
			spawn(server, checkAlias, [Pid, Alias])		       	      
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

