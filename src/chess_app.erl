-module(chess_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [{'_', game_handler, []}]}
    ]),

    {ok, _} = cowboy:start_http(http, 100, [{port, 5000}], [{env, [{dispatch, Dispatch}]}]),
    chess_sup:start_link().

stop(_State) ->
    ok.
