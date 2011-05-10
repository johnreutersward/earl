EBIN = ebin
SRC = src
ERLC = erlc -W -o $(EBIN)
SERVER = erl -name server -connect_all false -pa $(EBIN) -s server init
CLIENT = erl -noshell -name client -connect_all false -pa $(EBIN) -s client init

all:	build

build:
	@mkdir -p $(EBIN)
	$(ERLC) $(SRC)/*.erl
	@echo "All compiled"

doc:	all
		@mkdir -p doc
		erl -noshell -run edoc application (earl, *.erl
		echo "Not yet implemented"

client:	all
	@clear
	@$(CLIENT)
	
server:	all
	@clear	
	$(SERVER)
	
clean:
	@rm -fv $(EBIN)/*.beam
	@rm -fv erl_crash.dump

