-module(edoccer).
-export([init/0]).

init() -> edoc:application(earliweb, ".", []),
		  init:stop().
