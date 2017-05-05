PROJECT = erc

# deps
DEPS = reloader
# deps urls
dep_reloader = git git@git.ceb.loc:erlang/reloader.git master

# Compiler options.
ERLC_OPTS ?= -W1

include erlang.mk

.PHONY: debug

debug: app rel
