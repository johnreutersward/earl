CC = erlc -W
ERL = erl -noshell -s
ERL_SRV = erl -sname server -setcookie earl_game_club -s server init
ERL_CLI = erl -noshell -sname client -setcookie earl_game_club -s client start
EBIN_DIR = ebin
SRC_DIR = src
ERLC_COMPLETE = -o $(EBIN_DIR)

client:	client.beam client_handler.beam
	$(ERL_CLI)

server:	server.beam
	$(ERL_SRV) 

test:	build
	$(ERL) client runtest
	$(ERL) client_handler runtest
	$(ERL) server runtest

build:	client.beam client_handler.beam server.beam

client.beam:	client.erl
	$(CC) $<

client_handler.beam:	client_handler.erl
	$(CC) $<

server.beam:	server.erl
	$(CC) $<
    
complete:
	$(CC) $(ERLC_COMPLETE) $(SRC_DIR)/*.erl

clean:
	rm -f *.beam

