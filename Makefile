EBIN = ebin
SRC = src
ERLC = erlc -W -o $(EBIN)
SERVER = erl -sname server -connect_all false -pa $(EBIN) -s server init
CLIENT = erl -noshell -sname client -connect_all false -pa $(EBIN) -s client init
DOCCER = erl -noshell -pa $(EBIN) -s makeutils doccer
TESTER = erl -noshell -pa $(EBIN) -s makeutils tester

all:	build

build:
	@mkdir -p $(EBIN)
	$(ERLC) $(SRC)/*.erl
	@echo "All compiled"

doc: all
	@mkdir -p doc
	@echo "Generating edoc files"
	@$(DOCCER)
	@echo "All done"

client:	all
	@clear
	@$(CLIENT)
	
server:	all
	@clear	
	$(SERVER)
	
clean:
	@rm -fv $(EBIN)/*.beam
	@rm -fv erl_crash.dump

test: all
	@echo "Running testcases"
	$(TESTER)
	@echo "All done"


