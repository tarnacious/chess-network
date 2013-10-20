-module(worker).

%% api
-export([process/2]).

process(Pid,Message) ->
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, req),
    ok = erlzmq:connect(Socket,"tcp://localhost:5555"),
    ok = erlzmq:send(Socket, Message),
    {ok, Reply} = erlzmq:recv(Socket),
     
    io:format("chess response: [~p]~n", [Reply]),
    case Reply of 
      << "Parse Error", _ >> -> 
        io:format("Parse Error~n"),
        gen_server:call(Pid, {error, Reply});  
      << "Invalid Move", _ >> -> 
        io:format("Invalid Move~n"),
        gen_server:call(Pid, {invalid_move, Reply});  
      _ -> 
        io:format("New State~n"),
        gen_server:call(Pid, {process, Reply})  
    end,
    ok = erlzmq:close(Socket),
    ok = erlzmq:term(Context),
    ok.

