#!/bin/sh

exec erl -sname erc \
  -config sys \
  -boot start_sasl \
  -s erc
