EBIN_DEPS=ebin $(wildcard deps/*/ebin)
LIB_ARGS=$(EBIN_DEPS:%=-pa %)

.PHONY: all deps run

all:: priv/erlb.js priv/erws.boot

# See LICENSE for licensing information.

PROJECT = erws

# Options.

ERLC_OPTS ?= -Werror +debug_info +warn_export_all +warn_export_vars \
             +warn_shadow_vars +warn_obsolete_guard #+warn_missing_spec
PLT_APPS = crypto public_key ssl

# Dependencies.

DEPS = cowboy lager
dep_cowboy = git https://github.com/saleyn/cowboy.git master
dep_lager  = git https://github.com/basho/lager.git   master

# Standard targets.

include erlang.mk

# Also dialyze the tests.

# DIALYZER_OPTS += --src -r test

clean::
	rm -fr ebin log priv/erlb.js priv/erws.{rel,script,boot} erl_crash.dump

dist-clean::
	rm -fr deps priv/erlb.js priv/release.es priv/erws.{rel,script,boot} \
           .erlang.mk.packages* .rebar

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
	erl $(LIB_ARGS) -boot priv/erws
