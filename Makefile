EBIN_DEPS=$(wildcard deps/*/ebin)
DEPS_DIRS=$(EBIN_DEPS:%=-pa ./%)

.PHONY: rebar-check all deps compile run

all: compile priv/erws.boot priv/erlb.js

rebar-check:
	@if ! which rebar > /dev/null 2>&1 ; then \
   		echo "rebar not found in PATH. Get it at https://github.com/basho/rebar.git"; \
		exit 1; \
	fi

deps: rebar-check
	rebar get-deps compile

compile: rebar-check
	rebar compile skip_deps=true

clean:
	rm -fr ebin log

dist-clean: clean
	rm -fr deps

priv/erws.boot: priv/erws.rel
	erlc -pa ebin $(DEPS_DIRS) -o priv $<

priv/erlb.js:
	curl -s -o $@ https://raw.github.com/saleyn/erlb.js/master/erlb.js

run:
	erl -pa ebin $(DEPS_DIRS) -boot priv/erws
