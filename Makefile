EBIN_DEPS=ebin $(wildcard deps/*/ebin)
LIB_ARGS=$(EBIN_DEPS:%=-pa %)

.PHONY: rebar-check all deps compile run

all: compile priv/erlb.js priv/erws.boot

rebar-check:
	@if ! which rebar > /dev/null 2>&1 ; then \
   		echo "rebar not found in PATH. Get it at https://github.com/basho/rebar.git"; \
		exit 1; \
	fi

deps:
	rebar get-deps 
	$(MAKE) skip_deps=false all

compile: rebar-check
	rebar compile skip_deps=$(if $(skip_deps),$(skip_deps),true)

clean:
	rm -fr ebin log priv/erlb.js priv/erws.{rel,script,boot}

dist-clean: clean
	rm -fr deps priv/erlb.js priv/release.es priv/erws.{rel,script,boot}

priv/erws.boot: priv/erws.rel
	erlc $(LIB_ARGS) -o $(@D) $<
	
priv/erws.rel: src/erws.rel priv/release.es
	escript priv/release.es $< $@

priv/release.es:
	curl -s https://raw.github.com/saleyn/util/master/bin/release.es | \
	awk '/^%%!/ { print "%%! $(LIB_ARGS)" } !/^%%!/ {print}' > $@

priv/erlb.js:
	curl -s -o $@ https://raw.github.com/saleyn/erlb.js/master/erlb.js

run:
	erl $(LIB_ARGS) -boot priv/erws
