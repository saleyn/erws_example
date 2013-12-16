EBIN_DEPS=$(wildcard deps/*/ebin)
DEPS_DIRS=$(EBIN_DEPS:%=-pa ./%)

.PHONY: rebar-check all deps compile run

all: compile priv/erlb.js priv/erws.boot

rebar-check:
	@if ! which rebar > /dev/null 2>&1 ; then \
   		echo "rebar not found in PATH. Get it at https://github.com/basho/rebar.git"; \
		exit 1; \
	fi

deps:
	rm -f priv/erlb.js
	rebar get-deps
	$(MAKE) all

compile: rebar-check
	rebar compile skip_deps=true

clean:
	rm -fr ebin log priv/erlb.js priv/erws.{script,boot}

dist-clean: clean
	rm -fr deps priv/erlb.js priv/erws.{script,boot}

priv/erws.boot: priv/erws.rel
	erlc -pa ebin $(DEPS_DIRS) -o priv $<

priv/erlb.js:
	curl -s -o $@ https://raw.github.com/saleyn/erlb.js/master/erlb.js

run:
	erl -pa ebin $(DEPS_DIRS) -boot priv/erws
