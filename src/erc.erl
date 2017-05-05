-module(erc).

-export([
  start/0,
  stop/0
]).

start() ->
  application:start(erc).


stop() ->
    application:stop(erc).
