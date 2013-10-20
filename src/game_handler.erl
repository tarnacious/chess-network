-module(game_handler).
-behaviour(cowboy_http_handler).
-export([init/3, terminate/2]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    {Path, _} = cowboy_req:path(Req),
    lobby_server:open(Path),
    Req2 = cowboy_req:compact(Req),
    {ok, Req2, undefined, hibernate}.

websocket_handle({text, Msg}, Req, State) ->
    jiffy:decode(Msg),
    websocket_handle_({text, jiffy:decode(Msg)}, Req, State);

websocket_handle(_Any, Req, State) ->
    io:format("WHAT?~n"),
    {ok, Req, State}.

websocket_handle_({text, {[{<<"command">>,<<"message">>},
                           {<<"message">>,Message}]}}, Req, State) ->
    gen_server:call(State, {send_message, Message}),
    {ok, Req, State, hibernate};


websocket_handle_({text, {[{<<"command">>,<<"move">>},
                           {<<"move">>,Message}]}}, Req, State) ->
    io:format("Got move ~p~n", [Message]),
    gen_server:call(State, {move, Message}),
    {ok, Req, State, hibernate};

websocket_handle_({text, Anything}, Req, State) ->
    io:format("I do not understand, sorry ~p~n",[Anything]),
    {reply,
        {text, << "Sorry I don't understand." >>}, Req, State, hibernate
    }.

websocket_info({message, Msg}, Req, State) ->
    Json = jiffy:encode({[
                {<< "type" >>, << "message" >>},
                {<< "message" >>, << "Message: ", Msg/binary >>}]}),
    {reply,
        {text, Json}, Req, State, hibernate
    };

websocket_info({state, GameState}, Req, State) ->
    Json = jiffy:encode({[
                    {<< "type" >>, << "state" >>},
                    {<< "state" >>, << GameState/binary >>}]}),
    {reply,
        {text, Json}, Req, State, hibernate
    };
    
websocket_info({open, RouterPid, _Name}, Req, _State) ->
    io:format("Register CLient ~p~n", [RouterPid]),
    gen_server:call(RouterPid, register_client),
    {ok, Req, RouterPid, hibernate};

websocket_info(Info, Req, State) ->
    io:format("Un-matched websocket server info. ~p~n", [Info]),
    {ok, Req, State, hibernate}.

websocket_terminate(Reason, _Req, State) ->
    gen_server:call(State, unregister_client),
    ok.
