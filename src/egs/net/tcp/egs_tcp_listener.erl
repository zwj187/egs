%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% TCP监听器
%%% @end
%%%-------------------------------------------------------------------
-module(egs_tcp_listener).
-behaviour(gen_server).
-include("egs.hrl").
-include("egs_net.hrl").


%% API
-export([
  start_link/1,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  shutdown/0,
  code_change/3]).

%% @doc 启动监听器
-spec start_link(map()) -> {ok, pid()}.
start_link(A) when is_map(A) ->
%%  ?LOG_INFO("Start Tcp Listener:~w...", [A]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, A, []).

init(#{port := Port, count := ListenCount, ip := Ip}) ->
  process_flag(trap_exit, ?TRUE),
  {ok, #egs_tcp_session{port = Port, listen = ListenCount, ip = Ip}, 0}.

handle_call(socket, _From, State) -> {reply, get(socket), State};

handle_call(shutdown, _From, #egs_tcp_session{socket = LSocket} = State) ->
  gen_tcp:shutdown(LSocket, read_write),
  {reply, ok, State};

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(timeout, State = #egs_tcp_session{port = Port, listen = LCount}) ->
%%  ?LOG_INFO("Tcp Listener Start:~w", [Port]),
  case gen_tcp:listen(Port, ?TCP_OPTIONS) of
    {ok, LSock} ->
      erlang:put(socket, LSock),
      Session = #egs_tcp_session{socket = LSock, port = Port},
      lists:foreach(
        fun(Index) ->
          {ok, APid} = egs_tcp_sup:start_acceptor(Index, Session#egs_tcp_session{index = Index}),
          %% 启动接收器
          ?GO_LISTEN(APid)
        end, lists:seq(1, LCount)),
      {noreply, State#egs_tcp_session{socket = LSock}};

    {error, Reason} ->
      ?LOG_ERROR("Tcp Listener Start Error: ~p", [Reason]),
      {stop, {cannot_listen, Port, Reason}}
  end;

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, State = #egs_tcp_session{socket = LSock}) ->
  ?IF(is_port(LSock), gen_tcp:close(LSock), skip),
  {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

shutdown() ->
  ?TRY(gen_server:call(?MODULE, shutdown)),
  ok.
