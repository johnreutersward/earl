
CC = erlc -W
ERL = erl -noshell -s
ERL_SRV = erl -name server -setcookie earl_game_club -s server init
ERL_CLI = erl -noshell -name client -setcookie earl_game_club -s client init

client:	client.beam client_handler.beam
	$(ERL_CLI)

server:	server.beam database.beam
	$(ERL_SRV) 

test:	build
	$(ERL) client runtest
	$(ERL) client_handler runtest
	$(ERL) server runtest

build:	client.beam client_handler.beam server.beam database.beam

client.beam:	client.erl
	$(CC) $<

client_handler.beam:	client_handler.erl
	$(CC) $<

server.beam:	server.erl
	$(CC) $<
database.beam:	database.erl
	$(CC) $<

clean:
	rm -f *.beam

