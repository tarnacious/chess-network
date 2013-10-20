-module(chess).

-export([start_link/0, start/0, stop/0]).

start_link() ->
  ok.

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(chess),
  ok.

stop() ->
  application:stop(chess).

