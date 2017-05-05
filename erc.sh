#!/bin/sh

exec erl -sname erc \
  -pa ebin/ deps/*/ebin \
  -config sys \
  -boot start_sasl \
  -s reloader \
  -s erc
