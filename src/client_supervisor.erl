%% @author Andreas Hammar <andreashammar@ymail.com>
%% @doc A supervisor that messages the client_handler if a client dies.

-module(client_supervisor).
-export([init/1, loop/1]).

%% @doc Initiates the client_supervisor.
%% @spec init() -> ok

init(ClientHandler) -> 
	io:format("Starting...~n"),
	link(ClientHandler),
	process_flag(trap_exit, true),
	loop(ClientHandler).

%% @doc Notifies the client_handler if a client dies.
%% @spec loop() -> ok

loop(ClientHandler) ->
	receive
	{'EXIT', _Pid, _Reason} -> 
			    ClientHandler ! {quit},
			    init:stop()
	end.
