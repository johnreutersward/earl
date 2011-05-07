EBIN = ebin
SRC = src
ERLC = erlc -W -o $(EBIN)
SERVER = erl -sname server -setcookie earl_game_club -pa $(EBIN) -s server init
CLIENT = erl -noshell -sname client -setcookie earl_game_club -pa $(EBIN) -s client init

all:
	@mkdir -p $(EBIN)
	$(ERLC) $(SRC)/*.erl
	@echo "All compiled" 

client:
	$(CLIENT)
	
server:
	$(SERVER)
	
clean:
	@rm -fv $(EBIN)/*.beam
	@rm -fv erl_crash.dump

