%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% socket进程
%%% @end
%%%-------------------------------------------------------------------
-module(egs_tcp_socket).
-include("egs.hrl").
-include("egs_net.hrl").
-behaviour(egs_server).

-export([
  send_msg/2,
  flush/1,
  stop/2,
  start/1,
  handle_init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  handle_terminate/2
]).

-define(RESET_SOCKET_OPTIONS(Socket), inet:setopts(Socket, [binary, {active, 8}, {reuseaddr, true}, {keepalive, true}, {packet, 0}])).
-define(SEND_SOCKET_MSG, send_msg_to).
-define(SEND_SOCKET_TIMEOUT, 84600*7).   %% 数据N秒内超时将会给kill

-define(CACHE_BUFFER_TICK, socket_buffer_tick).
-define(CACHE_BUFFER_FLUSH, socket_buffer_flush).
-define(CACHE_BUFFER_SIZE, 4096).
-define(CACHE_BUFFER_TICK_TIME, 200).


%% export
send_msg(?UNDEFINED, _Msg) ->
  ?FAIL;
send_msg(SocketPID, Msg) ->
  SocketPID ! {?SEND_SOCKET_MSG, Msg}.

flush(SocketPID) ->
  SocketPID ! ?CACHE_BUFFER_FLUSH.

%% 启动socket
start(SocketArg) ->
  ?LOG_INFO("Start Socket:~p", [SocketArg]),
  egs_server:start(?MODULE,
    SocketArg,
    [{spawn_opt, [{min_heap_size, 10 * 1024}, {min_bin_vheap_size, 10 * 1024}]}]
  ).

%% 回调函数
handle_init({ClientSocket, Port}) ->
  process_flag(trap_exit, true),
  case inet:peername(ClientSocket) of
    {ok, {IP, _}} ->
      SocketState = #egs_tcp_socket{port = Port, ip = IP, last_package = egs_time:now_sec()},
      case ?TRY(egs_tcp_handler:handle_init(ClientSocket)) of
        {?OK, NetState} ->
          ?RESET_SOCKET_OPTIONS(ClientSocket),
          buffer_tick(),
          {?OK, SocketState#egs_tcp_socket{net_state = NetState}};

        Err ->
          ?LOG_ERROR("Socket Init Error:~p", [Err]),
          {?ERR_RET, ?SOCKET_ERROR_INIT}

      end;
    {error, Reason} ->
      ?LOG_ERROR("Socket Start Error:~p", [Reason]),
      {?ERR_RET, inet:format_error(Reason)}
  end;
handle_init(?UNDEFINED) ->
  {?OK, #egs_tcp_socket{net_state = #egs_tcp_handler{step = ?NET_STEP_CONNECT}}}.

%% call
handle_call(Info, _From, State) ->
  do_handle_socket_event(?SOCKET_EVENT_CALL, Info, State).

%% cast
handle_cast(Info, State) ->
  do_handle_socket_event(?SOCKET_EVENT_CAST, Info, State).

handle_info(stop, State = #egs_tcp_socket{}) ->
  {stop, normal, State};

handle_info(kill, State = #egs_tcp_socket{}) ->
  {stop, normal, State};

%% 接收到数据(FIXME 只支持websocket)
handle_info({tcp, _, DataBin}, State = #egs_tcp_socket{hand_shake = false}) ->
  ?IF(egs_websocket:check_websocket(DataBin),
    begin
      State1 = #egs_tcp_socket{net_state = NetState} = do_send_msg(egs_websocket:hand_shake(DataBin), State),
      do_handle_recv_data(<<>>, State1#egs_tcp_socket{hand_shake = ?TRUE, net_state = NetState#egs_tcp_handler{proto = ?PROTO_WS}})
    end,
    %% 握手失败
    {noreply, State});

%% 正常处理数据
handle_info({tcp, _, DataBin}, State) ->
  do_handle_recv_data(DataBin, State);

%% socket错误
handle_info({inet_async, _Socket, _Ref, {error, Reason}}, State) ->
  do_handle_socket_err(Reason, State);

%% 默认退出
handle_info({'EXIT', Socket, Reason}, State = #egs_tcp_socket{net_state = #egs_tcp_handler{socket = Socket}}) ->
  do_handle_socket_err(Reason, State);

handle_info({?SOCKET_ERROR_TCP_ERROR, _, Reason}, State ) ->
  do_handle_socket_err(Reason, State);

handle_info({?SOCKET_ERROR_TCP_CLOSED, _Socket}, State) ->
  do_handle_socket_err(?SOCKET_ERROR_TCP_CLOSED, State);

% tcp connection change to passive
handle_info({tcp_passive, Socket}, #egs_tcp_socket{net_state = #egs_tcp_handler{socket = Socket}} = State) ->
  ?RESET_SOCKET_OPTIONS(Socket),
  {noreply, State};

%% 发送信息
handle_info({?SEND_SOCKET_MSG, SendData}, State = #egs_tcp_socket{net_state = NetState}) ->
  case ?TRY(egs_tcp_handler:handle_send(SendData, NetState)) of
    {ok, Bin, NetState1} when is_binary(Bin); is_list(Bin) ->
      State2 = do_send_msg(Bin, State#egs_tcp_socket{net_state = NetState1}),
      {noreply, State2};
    _ ->
      {noreply, State}
  end;

handle_info(?CACHE_BUFFER_TICK, State = #egs_tcp_socket{}) ->
  buffer_tick(),
  State1 = do_send_buffer(State),
  {noreply, State1};

handle_info(?CACHE_BUFFER_FLUSH, State = #egs_tcp_socket{}) ->
  State1 = do_send_buffer(State),
  {noreply, State1};

handle_info({inet_reply, _, _Result}, State = #egs_tcp_socket{}) ->
  {noreply, State};

handle_info(check_socket, State = #egs_tcp_socket{net_state = #egs_tcp_handler{socket = Socket}}) ->
  ?LOG_INFO("~p", [erlang:port_info(Socket)]),
  {noreply, State};

handle_info(Info, State) ->
  do_handle_socket_event(?SOCKET_EVENT_INFO, Info, State).

handle_terminate(Reason, State) ->
  do_terminate(Reason, State),
  ok.

%% stop socket pid
stop(SocketPID, Reason) ->
  case egs_funs:is_proc_alive(SocketPID) of
    ?TRUE ->
      ?TRY(SocketPID ! {stop, Reason});
    _ ->
      ?IGNORE
  end.

%% 处理socket错误
do_handle_socket_err(Reason, #egs_tcp_socket{net_state = NetState} = State) ->
%%  ?LOG_WARNING("Socket Error:~p", [Reason]),
  case egs_tcp_handler:handle_error(Reason, NetState) of
    {?OK, NetState1} ->
      {noreply, State#egs_tcp_socket{net_state = NetState1}};

    {stop, Reason1} ->
      {stop, Reason1, State};

    _ ->
      ?LOG_ERROR("Socket Error, Close exception:~p", [Reason]),
      {stop, Reason, State}
  end.

%% 处理接受到的数据
do_handle_recv_data(<<>>, State) ->
  {noreply, State};
do_handle_recv_data(DataBin, State = #egs_tcp_socket{net_state = NetState}) ->
  case ?TRY(egs_tcp_handler:handle_recv(DataBin, NetState)) of
    {ok, NetState1} ->
      {noreply, State#egs_tcp_socket{net_state = NetState1, last_package = egs_time:now_sec()}};
    {stop, _Reason} ->
      {stop, normal, State};
    {stop, _Reason, _NetState} ->
      {stop, normal, State};
    Reason ->
      ?LOG_ERROR("Socket Exception:~w ~w", [Reason, State]),
      {noreply, State}
  end.

%% 事件处理
%% EventID == call cast info
do_handle_socket_event(EventID, Msg, State = #egs_tcp_socket{net_state = NetState}) ->
  case egs_tcp_handler:handle_event(?SOCKET_EVENT(EventID, Msg), NetState) of
    stop ->
      {stop, normal, State};

    {stop, _R} ->
      {stop, normal, State};

    {stop, _, _} ->
      {stop, normal, State};

    {ok, Result, NetState1} ->
      State1 = State#egs_tcp_socket{net_state = NetState1},
      {reply, Result, State1};

    {ok, NetState1} ->
      State1 = State#egs_tcp_socket{net_state = NetState1},
      {noreply, State1};

    _ ->
      ?IF(EventID == ?SOCKET_EVENT_CALL,
        {reply, ?FAIL, State},
        {noreply, State})
  end.

do_terminate(Reason, State = #egs_tcp_socket{net_state = NetState}) ->
  catch egs_tcp_handler:handle_terminate(Reason, NetState),
  %% 让没做的事，做完
  ?SLEEP(100),
  catch do_send_buffer(State),
  %% socket有可能要传递给第三方，所以不关闭
  %% catch gen_tcp:close(NetState#handler_state.socket),
  ok.

%%
buffer_tick() ->
  erlang:send_after(?CACHE_BUFFER_TICK_TIME, self(), ?CACHE_BUFFER_TICK).

%% cache msg
%% send msg
do_send_msg(Bin, State = #egs_tcp_socket{net_state = #egs_tcp_handler{socket = Socket}, package = Buffer}) ->
  %%
  Buffer1 = [Buffer, Bin],
  NowTime = egs_time:now_sec(),
  case iolist_size(Buffer1) >= ?CACHE_BUFFER_SIZE  andalso is_port(Socket) of
    ?TRUE ->
%%      catch erlang:port_command(Socket, Buffer1),
      ?TRY(erlang:port_command(Socket, Buffer1)),
      State#egs_tcp_socket{package = [], last_package = NowTime};
    ?FALSE ->
      State#egs_tcp_socket{package = Buffer1, last_package = NowTime}
  end.

%% clear buffer
do_send_buffer(State = #egs_tcp_socket{last_package = LastTime, net_state = #egs_tcp_handler{socket = Socket} = NetState, package = Buffer}) ->
  NowTime = egs_time:now_sec(),
  case iolist_size(Buffer) > 0 andalso is_port(Socket) of
    ?TRUE ->
      catch erlang:port_command(Socket, Buffer),
      State#egs_tcp_socket{package = [], last_package = NowTime};
    _ ->
      %% beat
      %% 超时Kill
      do_socket_time_out(LastTime, NowTime, NetState),
      State#egs_tcp_socket{package = []}
  end.


%% 数据包超时
do_socket_time_out(LastPackageTime, NowTime, State) ->
  TimeLimit = LastPackageTime + ?SEND_SOCKET_TIMEOUT,
  ?IF(TimeLimit < NowTime,
    begin
      ?LOG_WARNING("Socket Data TimeOut: ~p", [State]),
      catch gen_tcp:close(State#egs_tcp_handler.socket),
      self() ! ?STOP
    end,
    ?OK
  ).