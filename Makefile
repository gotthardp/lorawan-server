# See LICENSE for licensing information.
PROJECT=lorawan-server
VERSION=0.1.0
REBAR = ./rebar3

PRE17 := $(shell ERL_FLAGS="" erl -eval 'io:format("~s~n", [case re:run(erlang:system_info(otp_release), "^R") of nomatch -> ""; _ -> pre17 end]), halt().' -noshell)

.PHONY: compile release version start clean

all: clean release

compile:
	@$(REBAR) compile

release:
	@$(REBAR) release

## change the version of the app
version:
	@echo "Setting version:$(VERSION)"
	perl -p -i -e "s/^\s*{vsn,.*/    {vsn, \"$(VERSION)\"},/g" src/lorawan_server.app.src
	perl -p -i -e "s/^\s*{release,.*/    {release, {\'lorawan-server\', \"$(VERSION)\"},/g" rebar.config
	@echo "Done"

start:
	./_build/default/rel/${PROJECT}/bin/${PROJECT}

clean:
	@$(REBAR) clean

.PHONY: package test

## release package
package:
	@$(REBAR) tar -n ${PROJECT} -i true

test:
	@$(REBAR) eunit

ifdef DEBUG
EXTRA_OPTS:=debug_info,
endif

ifndef PRE17
EXTRA_OPTS:=$(EXTRA_OPTS) {d,namespaced_types},
endif
