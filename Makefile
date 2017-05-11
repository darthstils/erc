PROJECT = erc

# deps
DEPS = reloader eredis
# deps urls
dep_reloader = git git@git.ceb.loc:erlang/reloader.git master
dep_reloader = git git@github.com:wooga/eredis.git master

# Compiler options.
ERLC_OPTS ?= -W1

include erlang.mk

.PHONY: debug

debug: app rel
