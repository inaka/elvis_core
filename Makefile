PROJECT = elvis

DEPS = lager zipper katana_code
TEST_DEPS = katana_test mixer meck
SHELL_DEPS = sync
BUILD_DEPS = inaka_mk hexer_mk
DEP_PLUGINS = inaka_mk hexer_mk

dep_lager       = git https://github.com/basho/lager.git         3.1.0
dep_zipper      = hex 0.2.0
dep_katana_code = git https://github.com/inaka/katana-code.git   0.0.3
dep_katana_test = git https://github.com/inaka/katana-test.git   0.0.5
dep_mixer       = git https://github.com/inaka/mixer.git         0.1.5
dep_meck        = git https://github.com/eproxus/meck            0.8.4
dep_sync        = git https://github.com/rustyio/sync.git        11df81d
dep_inaka_mk    = git https://github.com/inaka/inaka.mk.git      1.0.0
dep_hexer_mk    = git https://github.com/inaka/hexer.mk.git      1.1.0

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
