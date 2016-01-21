PROJECT = elvis

DEPS = lager zipper katana

dep_lager  = git https://github.com/basho/lager.git 3.0.2
dep_zipper = hex 0.1.4
dep_katana = hex 0.2.21

TEST_DEPS = meck

dep_meck = git https://github.com/eproxus/meck 0.8.3

SHELL_DEPS = sync

dep_sync = git https://github.com/rustyio/sync.git 9c78e7b

include erlang.mk

# Avoid erlang.mk download katana's elvis_core dependency
IGNORE_DEPS += elvis_core

ERLC_OPTS += +'{parse_transform, lager_transform}'
ERLC_OPTS += +warn_unused_vars +warn_export_all +warn_shadow_vars +warn_unused_import +warn_unused_function
ERLC_OPTS += +warn_bif_clash +warn_unused_record +warn_deprecated_function +warn_obsolete_guard +strict_validation
ERLC_OPTS += +warn_export_vars +warn_exported_vars +warn_missing_spec +warn_untyped_record +debug_info

# Commont Test Config

TEST_ERLC_OPTS += +'{parse_transform, lager_transform}'
CT_OPTS = -cover test/elvis.coverspec -erl_args -config config/test.config
SHELL_OPTS = -name ${PROJECT}@`hostname` -s lager -s sync -s elvis_core -config config/elvis.config

test-shell: build-ct-suites app
	erl -pa ebin -pa deps/*/ebin -name ${PROJECT}-test@`hostname` -pa test -s lager -s sync -s elvis_core -config config/test.config

quicktests: ERLC_OPTS = $(TEST_ERLC_OPTS)
quicktests: clean app build-ct-suites
	@if [ -d "test" ] ; \
	then \
		mkdir -p logs/ ; \
		$(CT_RUN) -suite $(addsuffix _SUITE,$(CT_SUITES)) $(CT_OPTS) ; \
	fi
	$(gen_verbose) rm -f test/*.beam
