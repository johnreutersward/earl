-module(master_supervisor).
-behaviour(supervisor).
-export([init/1]).
-export([start/0]).

start() -> 
	io:format("starting...~n"),
	Pid = spawn_link(server, server, []), 
	{ok, Pid}.

init(_) ->
	{ok, {{one_for_one, 30,60},
	[{master_supervisor, {master_supervisor, start, []},
		permanent, brutal_kill, worker, [master_supervisor]}]}}.
