.PHONY: all deps run

PROJECT = erws

# Options.

# Dependencies.

# Standard targets.

all:: priv/erlb.js # priv/erws.boot
	rebar3 compile

release:
	APP_ROOT=var rebar3 release

deps:
	rebar3 get-deps

# Also dialyze the tests.

# DIALYZER_OPTS += --src -r test

clean::
	rm -fr ebin log priv/erlb.js priv/erws.{rel,script,boot} erl_crash.dump

dist-clean distclean:
	rm -fr deps priv/erlb.js priv/release.es priv/erws.{rel,script,boot} \
           .erlang.mk.packages* .rebar _build

priv/erws.boot: priv/erws.rel
	erlc $(LIB_ARGS) -o $(@D) $<
	
priv/erws.rel: src/erws.rel priv/release.es
	escript priv/release.es $< $@

priv/release.es:
	wget -q -O - https://raw.github.com/saleyn/util/master/bin/release.es | \
	awk '/^%%!/ { print "%%! $(LIB_ARGS)" } !/^%%!/ {print}' > $@

priv/erlb.js:
	wget -O $@ -q https://raw.github.com/saleyn/erlb.js/master/erlb.js

src/erws.rel:
	relx --verbose=0
	cp _rel/releases/1.0/erws.rel $@
	rm -fr _rel

run:
	@#erl $(LIB_ARGS) -boot priv/erws
	APP_ROOT=var rebar3 shell
