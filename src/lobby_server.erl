-module(lobby_server).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% api
-export([start_link/0, stop/0]).
-export([open/1, register/0]).

%% gen_server 
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
  gen_server:cast(?SERVER, stop).
    
register() ->
  gen_server:call(?SERVER, register ).

open(Name) ->
  gen_server:call(?SERVER, {open, Name}).

init([]) ->
  {ok, { dict:new(), [] }}.

handle_call(game_closed, _From, {Games, Clients}) ->
  {GamePid, _} = _From,
  FilteredGames = dict:filter( fun (_,Pid) -> 
                GamePid /= Pid
                end, Games),
  {reply, ok,{FilteredGames, Clients}};
    

handle_call({open, Name}, _From, {Games, Clients}) ->
    {Pid, _} = _From,    
    Find =  dict:find(Name, Games),
    case Find of 
        error ->
          {ok, GamePid} = gen_server:start(game_server,{self(), [], { undefined, undefined, undefined }},[]),
          NewGames = dict:store(Name, GamePid, Games);
        _ ->
          NewGames = Games
     end,
  {ok, FindGamePid} =  dict:find(Name, NewGames),
  Pid ! {open, FindGamePid, Name},
  {reply, ok, {NewGames, Clients}};

handle_call(_Req, _From, {Games, Clients}) ->
  {reply, ok, {Games, Clients}}.

handle_cast(_Req, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



