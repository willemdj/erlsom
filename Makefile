APPLICATION := erlsom

ERL := erl
EPATH := -pa ebin
TEST_EPATH := -pa .eunit

DIALYZER=dialyzer
DIALYZER_OPTS=-Wno_return -Wrace_conditions -Wunderspecs -Wbehaviours
PLT_FILE=.erlsom_plt
APPS=kernel stdlib

.PHONY: all clean test

all: compile

compile:
	@./rebar compile

doc:
	@./rebar doc

clean:
	@./rebar clean

build-plt: compile
	@./rebar build-plt

check-plt: compile
	@./rebar check-plt

dialyze:
	@./rebar dialyze

eunit:
	@./rebar eunit

shell: compile
	$(ERL) -sname $(APPLICATION) $(EPATH)

touch:
	find . -name '*' -print | xargs touch -m
	find . -name '*.erl' -print | xargs touch -m
