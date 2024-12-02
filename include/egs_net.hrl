%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 网络
%%% @end
%%%-------------------------------------------------------------------

-ifndef(EGS_NET_H_H).
-define(EGS_NET_H_H, true).

%% 网络相关
-define(TCP_OPTIONS, [
  binary
  , {packet, 0}
  , {active, 8}
  , {reuseaddr, true}
  , {nodelay, false}
  , {delay_send, true}
  , {high_watermark, 128 * 1024}
  , {low_watermark, 64 * 1024}
  , {sndbuf, 16 * 1024}
  , {recbuf, 16 * 1024}
  , {exit_on_close, true}
  , {send_timeout, 5000}
  , {keepalive, true}
  , {backlog, 30000}
  , {send_timeout_close, false}
]).

-record(egs_tcp_session, {
  socket,
  ip,
  port,
  listen,
  data,
  index
}).

-record(egs_tcp_socket, {
  port,
  ip,
  package = [],
  last_package = 0,
  hand_shake = false,
  net_state
}).

%% Listener 事件
-define(GO_LISTEN_EVENT, {event, listen}).
-define(GO_LISTEN(PID), (PID ! ?GO_LISTEN_EVENT)).

%% 网络事件
-define(NET_STEP_CONNECT, 0).
-define(NET_STEP_GAME_LOGIN, 1).
-define(NET_STEP_GAME_ONLINE, 2).
-define(NET_STEP_GAME_LOGOUT, 3).

%% 协议类型
-define(PROTO_TCP, 0). %% tcp
-define(PROTO_WS, 1).  %% web socket

-record(egs_tcp_handler, {
  socket
  ,proto = 0
  ,length = 0
  ,ws_data = <<>>
  ,step = ?NET_STEP_CONNECT
  ,last_login_ip
  ,last_login_time
  ,player
}).

%% socket事件
-define(SOCKET_EVENT(EVENT, MSG), {tcp_socket_event, EVENT, MSG}).
-define(SOCKET_EVENT_CALL, call).
-define(SOCKET_EVENT_CAST, cast).
-define(SOCKET_EVENT_INFO, info).

%% socket 错误
-define(SOCKET_ERROR_INIT, socket_init_error).
-define(SOCKET_ERROR_TCP_CLOSED, tcp_closed).
-define(SOCKET_ERROR_TCP_ERROR, tcp_error).








-endif.