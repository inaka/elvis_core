PROJECT = elvis

DEPS = lager zipper katana

dep_lager = git https://github.com/basho/lager.git 3.0.2
dep_zipper = git https://github.com/inaka/zipper 0.1.2
dep_katana =  git https://github.com/inaka/erlang-katana 0.2.13

TEST_DEPS = meck

dep_meck = git https://github.com/eproxus/meck 0.8.3

include erlang.mk

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_OPTS = -cover test/elvis.coverspec -erl_args -config config/test.config

# Builds the elvis escript.
escript: all
	rebar escriptize
	./elvis help

shell: app
	erl -pa ebin -pa deps/*/ebin -name elvis@`hostname` -s sync -s elvis -s lager -config config/elvis.config

test-shell: build-ct-suites app
	erl -pa ebin -pa deps/*/ebin -name elvis-test@`hostname` -pa test -s sync -s elvis -s lager -config config/test.config

install: escript
	cp elvis /usr/local/bin

quicktests: ERLC_OPTS = $(TEST_ERLC_OPTS)
quicktests: clean app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam
