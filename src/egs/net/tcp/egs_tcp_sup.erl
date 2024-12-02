%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% TCP监控树
%%% @end
%%%-------------------------------------------------------------------
-module(egs_tcp_sup).
-behaviour(supervisor).
-include("egs.hrl").
%% API
-export([
  start/0,
  start_link/0,
  stop/0,
  init/1,
  start_listener/1,
  start_acceptor/2
]).

-define(DEF_TCP_LISTENER, egs_tcp_listener).
-define(DEF_TCP_ACCEPTOR, egs_tcp_acceptor).

start() ->
  ?START_CHILD_SUP(?ROOT_SUP, ?MODULE, start_link, []).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  exit(?MODULE, shutdown),
  ok.

%% 启动监听器
start_listener(Args) ->
  start_listener(?DEF_TCP_LISTENER, Args).

-spec start_listener(Mod ::term(), Args::term()) -> ok.
start_listener(Mod, Args) ->
  supervisor:start_child(?MODULE,
    {
      Mod,
      {Mod, start_link, [Args]},
      transient,
      infinity,
      worker,
      [Mod]
    }).

%% 启动接收器
start_acceptor(Index, Session) ->
  supervisor:start_child(?MODULE,
    {
      egs_conv:to_atom(lists:concat([?DEF_TCP_ACCEPTOR, "_", Index])),
      {?DEF_TCP_ACCEPTOR, start_link, [Session]},
      transient,
      infinity,
      worker,
      [?DEF_TCP_ACCEPTOR]
    }).


init([]) ->
  {ok, {{one_for_one, 10, 10}, []}}.
