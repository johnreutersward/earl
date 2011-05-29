-module(rockpaperscissors).
-export([init/1, nextTurn/3, checkFinished/2]).


init(Players) ->
	gameAPI:print("Welcome to Rock Paper Scissors\n", Players),
	{Pid1, _} = lists:nth(1, Players),
	{Pid2, _} = lists:nth(2, Players),
	{{Pid1, noMove}, {Pid2, noMove}}.

nextTurn({{P1Pid, P1Move}, {P2Pid, P2Move}}, {PlayerPid, PlayerAlias}, Players) -> 
	OtherPlayer = lists:keydelete(PlayerPid, 1, Players),
	gameAPI:print("Make your move!", [{PlayerPid, PlayerAlias}]),
	gameAPI:print(PlayerAlias ++ "'s turn", OtherPlayer),
	PlayerMove = playerMove(PlayerPid, PlayerAlias),
	case PlayerPid of
		P1Pid ->
			{{P1Pid, PlayerMove}, {P2Pid, P2Move}};
		P2Pid ->
			{{P1Pid, P1Move}, {P2Pid, PlayerMove}}
	end.

playerMove(PlayerPid, PlayerAlias) ->
	case (gameAPI:getInput(PlayerPid)) of
		"rock" -> 
			rock;
		"paper" ->
			paper;
		"scissors" ->
			scissors;
		_ ->
			gameAPI:print("Invalid option", [{PlayerPid, PlayerAlias}]),
			playerMove(PlayerPid, PlayerAlias)	
	end.

checkFinished({{P1Pid, P1Move}, {P2Pid, P2Move}}, Players) ->
	if
	   	P1Move == noMove orelse P2Move == noMove ->
			{false};
		P1Move == P2Move ->
			{draw};
		true ->
			P1 = lists:keyfind(P1Pid, 1, Players), 
			P2 = lists:keyfind(P2Pid, 1, Players),
			case P1Move of
				rock when P2Move == paper ->
					Winner = P2;
				rock when P2Move == scissors ->
					Winner = P1;

				paper when P2Move == scissors ->
					Winner = P2;
				paper when P2Move == rock ->
					Winner = P1;

				scissors when P2Move == rock ->
					Winner = P2;
				scissors when P2Move == paper ->
					Winner = P1
			end,
			{true, Winner}
	end.	





