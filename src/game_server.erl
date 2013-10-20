-module(game_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% api
-export([start_link/0, stop/0, send_message/1]).
-export([register/0, unregister/0, server_info/0]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).
    
send_message(MessageBody) ->
  gen_server:call(?SERVER, {send_message, MessageBody}).

server_info() ->
  gen_server:call(?SERVER, user_count).

register() ->
  gen_server:call(?SERVER, register_client).

unregister() ->
  gen_server:call(?SERVER, unregister_client).

init({Pid,[], {_, Player1, Player2}}) ->
    {ok, Data} = file:read_file("priv/initial.txt"),
    {ok, {Pid,[], {Data, Player1, Player1}}}.

handle_call({send_message, MessageBody}, _From, {Pid, Clients, Game}) ->
  lists:foreach(fun(Client) -> Client ! {message, MessageBody} end, Clients),
  {reply, ok, {Pid, Clients, Game}, 1000};

handle_call(register_client, _From, {Pid, Clients, Game}) ->
  {ClientPid, _} = _From,
  {Board, _, _} = Game,
  ClientPid ! {state, Board},
  {reply, ok, {Pid, [ClientPid|Clients], Game}, 1000};

handle_call({move, Move}, _From, {Pid, Clients, Game}) ->
  {Board, _, _} = Game, 
  State = << Board/binary, " action: ", Move/binary >>,
  Me = self(),
  spawn(fun () -> worker:process(Me, State) end),
  {reply, ok, {Pid, Clients, Game}, 1000};

handle_call({join, player1}, _From, {Pid, Clients, Game}) ->
  {State, Player1, Player1} = Game,
  {reply, ok, {Pid, Clients, {State, _From, Player1}}, 1000};

handle_call({join, player2}, _From, {Pid, Clients, Game}) ->
  {State, Player1, Player1} = Game,
  {reply, ok, {Pid, Clients, {State, Player1, _From}}, 1000};

handle_call({process, State}, _From, {Pid, Clients, Game}) ->
  {_, Player1, Player2} = Game,
  lists:foreach(fun(Client) -> Client ! {state, State} end, Clients),
  {reply, ok, {Pid, Clients, {State, Player1, Player2}}, 1000};

handle_call({parse_error, _}, _From, {Pid, Clients, Game}) ->
  {State, _, _} = Game,
  lists:foreach(fun(Client) -> Client ! {message, << "Parse Error" >>} end, Clients),
  lists:foreach(fun(Client) -> Client ! {state, State} end, Clients),
  {reply, ok, {pid, Clients, Game}, 1000};

handle_call({invalid_move, _}, _From, {Pid, Clients, Game}) ->
  {State, _, _} = Game,
  lists:foreach(fun(Client) -> Client ! {message, << "Invalid Move" >>} end, Clients),
  lists:foreach(fun(Client) -> Client ! {state, State} end, Clients),
  {reply, ok, {pid, Clients, Game}, 1000};

handle_call(unregister_client, _From, {Pid, Clients, Game}) ->
  {ClientPid, _} = _From,
  {reply, ok, {Pid, lists:delete(ClientPid, Clients), Game}, 1000};

handle_call(user_count, _From, {Pid, Clients, Game}) ->
  {ClientPid, _} = _From,
  Count = length(Clients), 
  ClientPid ! {message, integer_to_list(Count)},
  {reply, ok, {Pid, Clients, Game}, 1000 };

handle_call(_Req, _From, {Pid, Clients, Game}) ->
  {reply, ok, {Pid, Clients, Game}, 1000}.

handle_cast(_Req, State) ->
  {noreply, State, 1000}.


handle_info(timeout, {Pid, Clients, Game}) ->
  case length(Clients) of 
      0 -> 
          io:format("No more clients.. I'm outski!~n"),
          gen_server:call(Pid, game_closed),
          {stop, normal, {Pid, Clients, Game}};  
      _ -> 
          {noreply, {Pid, Clients, Game}, 1000}
  end;


handle_info(_Info, State) ->
  {noreply, State, 1000}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State, 1000}.
