REBAR=$(shell which rebar || ./rebar)
SYNC_PATH = $(ERL_LIBS)/sync
PROPER_PATH = $(ERL_LIBS)/proper

all: deps compile

compile:
		@$(REBAR) compile

app:
		@$(REBAR) compile skip_deps=true

deps:
		@$(REBAR) get-deps

clean:
		@$(REBAR) clean

distclean: clean
		@$(REBAR) delete-deps

eunit: app
		@$(REBAR) eunit skip_deps=true

ct: app
		@$(REBAR) ct skip_deps=true

test: eunit ct

start:
		if test -d $(SYNC_PATH); \
		then exec erl -setcookie json -pa $(PWD)/deps/*/ebin -pa $(PWD)/ebin -boot start_sasl -s topical; \
		else exec erl -setcookie json -pa $(PWD)/deps/*/ebin -pa $(PWD)/ebin -boot start_sasl -s topical; \
		fi
