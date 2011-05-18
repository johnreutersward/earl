-module(makeutils).
-export([doccer/0, tester/0]).

doccer() -> edoc:application(earliweb, ".", []),
		  init:stop().

%% Add your modules with complete test-suites. Make sure you have a function runtest/0:
%% runtest() ->
%%		test(),
%%		init:stop().
%%	  
tester() -> client_handler:runtest(),
			init:stop().
