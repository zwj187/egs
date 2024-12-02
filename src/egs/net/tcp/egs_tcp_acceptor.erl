%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% TCP连接器
%%% @end
%%%-------------------------------------------------------------------
-module(egs_tcp_acceptor).
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
  code_change/3]).


start_link(Session) ->
%%  ?LOG_INFO("Start Tcp Acceptor:~w", [Session]),
  gen_server:start_link(?MODULE, Session, []).


init(Session) ->
  erlang:process_flag(trap_exit, true),
  erlang:register(egs_conv:to_atom(lists:concat([?MODULE, "_", Session#egs_tcp_session.index])), self()),
  {ok, Session}.

%% 进入监听
handle_info(?GO_LISTEN_EVENT, #egs_tcp_session{socket = LSock} = State) ->
%%  ?LOG_INFO("Acceptor OK ~p ~p~n",[LSock,self()]),
  case catch gen_tcp:accept(LSock) of
    {ok, ClientSock} ->
%%      ?LOG_INFO("Acceptor Client:~p", [ClientSock]),
      case inet:peername(ClientSock) of
        {ok, {IP, Port}} ->
          start_socket(ClientSock, IP, Port);
        _PErr ->
          ?LOG_ERROR("Get Client IP Error: ~p", [_PErr]),
          ?IGNORE
      end,

      %%
      %% 再次监听
      %%
      ?GO_LISTEN(self()),

      {noreply, State};

    {error, Reason} ->
      ?LOG_ERROR("Acceptor Error: ~p", [Reason]),
      {stop, normal, State}

  end;

handle_info(_Info, State) -> {noreply, State}.

handle_call(_Request, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%% 启动socket
start_socket(CSocket, _IP, Port) ->
  case egs_tcp_socket:start({CSocket, Port}) of
    {ok, CPid} ->
%%       ?LOG_INFO("Client Connect...[~p ~p ~p]", [CSocket,_IP, Port]),
      case gen_tcp:controlling_process(CSocket, CPid) of
        ok -> ?OK;
        Err ->
          ?LOG_ERROR("Controlling Process Error: ~p", [Err]),
          exit(CPid, kill)
      end;
    {error, Error} ->
      ?LOG_ERROR("Start Client Error: ~p", [Error])
  end.