%% @author Andreas Hammar <andreashammar@ymail.com>
%% @doc Spawns a supervisor for the server using the built in supervisor module.

-module(master_supervisor).
-behaviour(supervisor).
-export([init/1, run/0]).
-export([start/0]).

%% @doc Spawns and links the server.
%% @spec start() -> {ok, Pid}

start() -> 
	io:format("starting...~n"),
	Pid = spawn_link(server, server, []), 
	{ok, Pid}.

init(_) ->
	{ok, {{one_for_one, 30,60},
	[{master_supervisor, {master_supervisor, start, []},
		permanent, brutal_kill, worker, [master_supervisor]}]}}.

%% @doc Initiates the client_supervisor.
%% @spec run() -> ok

run() ->
	supervisor:start_link(client_supervisor, []),
	ok.
