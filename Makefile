# rebar3 logic by Bjorn-Egil Dahlberg
# https://gist.github.com/psyeugenic/d2d53a15463b218fd20c2fe7fd73ced0
REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3
endif

.PHONY: build upgrade clean distclean test release dist dpkg

build: $(REBAR3)
	@$(REBAR3) compile

$(REBAR3):
	wget $(REBAR3_URL) || curl -Lo rebar3 $(REBAR3_URL)
	@chmod a+x rebar3

upgrade: $(REBAR3)
	@$(REBAR3) upgrade

clean: $(REBAR3)
	@$(REBAR3) clean --all
	rm -rf node_modules

test: $(REBAR3)
	@$(REBAR3) eunit

release: $(REBAR3)
	@$(REBAR3) release

dist: $(REBAR3)
	@$(REBAR3) tar

dpkg:
	./scripts/dpkg-deb/build-deb

# end of file
