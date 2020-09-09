SHELL := bash
.ONESHELL:
.SHELLFLAGS := -euc
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

PWD=$(shell pwd)

ct: ci-before_install
ct: ci-install
ct: ci-script
.PHONY: ci-script

ci-before_install:
	@./ci before_install $(PWD)/rebar3
.PHONY: ci-before_install

ci-install:
	@./ci install $(PWD)/rebar3
.PHONY: ci-install

ci-script:
	@./ci script $(PWD)/rebar3
.PHONY: ci-script
