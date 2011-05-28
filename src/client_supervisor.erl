%% @author Tobias.Ericsson.0701@student.uu.se
%% @author Andreas.Hammar.5781@student.uu.se
%% @author Gabriella.Lundborg.6304@student.uu.se
%% @author Emma.Rangert.2142@student.uu.se
%% @author John.Reuterswärd.8971@student.uu.se
%% @author Simon.Young.0963@student.uu.se
%% @doc A supervisor that messages the client_handler if a client dies.

-module(client_supervisor).
-export([init/2, trap/2]).

%% @doc Initiates the client_supervisor to trap exits from ClientHandlerPid.
%% @spec init(ServerPid, ClientHandlerPid) -> ok

init(ServerPid, ClientHandlerPid) -> 
	link(ClientHandlerPid),
	process_flag(trap_exit, true),
	trap(ServerPid,ClientHandlerPid).

%% @doc Notifies the server with PID ServerPid if the client dies.
%% @spec trap(ServerPid, ClientHandlerPid) -> ok

trap(ServerPid,ClientHandlerPid) ->
	receive
	{'EXIT', _Pid, _Reason} -> 
			    ServerPid ! {quit, ClientHandlerPid}
	end.
