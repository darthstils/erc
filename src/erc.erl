-module(erc).

-export([
  start/0,
  stop/0,
  config/2
]).

start() ->
  application:start(erc).


stop() ->
    application:stop(erc).


config(_,_) ->
  [].
