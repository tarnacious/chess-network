Chess Messaging Server
=====

Simple (and incomplete) web socket server that defers all the game logic to
[another process][chessengine] via a ZeroMQ socket.

Server
------

Requirements:

    erlang
    rebar
    0mq

Get dependences:

    rebar get-deps

To run:

    make start 

Test Client
-----------

Requirements:

    node, npm


Get dependences:

    cd priv && npm install 

Run:
    
    cd priv && node play.js

[chessengine]: https://github.com/tarnacious/chess-engine 
