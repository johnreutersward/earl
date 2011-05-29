%% @author Tobias.Ericsson.0701@student.uu.se
%% @author Andreas.Hammar.5781@student.uu.se
%% @author Gabriella.Lundborg.6304@student.uu.se
%% @author Emma.Rangert.2142@student.uu.se
%% @author John.Reuterswärd.8971@student.uu.se
%% @author Simon.Young.0963@student.uu.se
%% @doc A supervisor that restarts a game room if it dies and updates the database.

-module(gameRoom_supervisor).
-export([init/2, linkRooms/3, trap/2, restart_room/2]).

%% @doc Initiates the supervisor to trap and handle exits and starts the spawnRooms function.
%% @spec init(GameList, DbPid) -> GameList

init(Gamelist, DbPid) ->
	process_flag(trap_exit, true),
	linkRooms(Gamelist, [], DbPid).

%% @doc Links all gameRooms in GameList to this supervising process.
%% @spec linkRooms(GameList, ResultingList, DbPid) -> ok

linkRooms([], ResultingList, DbPid) -> 
	trap(ResultingList, DbPid);
linkRooms([{GameModule, DisplayName, GamePid} | Tail], ResultingList, DbPid) ->
	link(GamePid),
	linkRooms(Tail, [{GameModule, DisplayName, GamePid} | ResultingList], DbPid).
	

%% @doc Traps exits and restarts any dying gameroom and updates the database on DbPid with a new GameList for any restarted gameRoom.
%% @spec trap(GameList, DbPid) -> ok 

trap(GameList, DbPid) ->
	receive
		{'EXIT', Pid, _Reason} ->
			NewGameList = restart_room(Pid, GameList),
			DbPid ! {setGamesList, NewGameList},
			trap(NewGameList, DbPid)
	end.

%% @doc restarts the gameRoom with Pid and updates its corresponding place in the Gamelist.
%% @spec restart_room(Pid, GameList) -> GameList

restart_room(Pid, GameList) ->
	NewGameTuple = lists:keyfind(Pid, 3, GameList),
	lists:keyreplace(Pid, 3, GameList, NewGameTuple).
