%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 消息处理器 FIXME 只支持websocket
%%% @end
%%%-------------------------------------------------------------------
-module(egs_tcp_handler).
-include("egs.hrl").
-include("egs_net.hrl").

%% API
-export([
  handle_init/1,
  handle_send/2,
  handle_recv/2,
  handle_terminate/2,
  handle_event/2,
  handle_error/2
]).

%% socket初始化
handle_init(Socket) ->
  {ok, {IP, _}} = inet:peername(Socket),
  NowTime = egs_time:now_sec(),
  {ok, #egs_tcp_handler{socket = Socket, last_login_ip = IP, last_login_time = NowTime}}.


%% 发送数据前，包装数据
%%  托管状态不发数据
handle_send(SendData, #egs_tcp_handler{} = NetState) ->
  SendData1 = egs_websocket:build_frame(SendData),
  {ok, SendData1, NetState};

handle_send(_SendData, _) ->
  ?IGNORE.

%% 收到数据，对数据进行解析
%% 检测counter是否异常
handle_recv(DataBin, #egs_tcp_handler{ws_data = WsData} = NetState) ->
  case egs_websocket:unmask_data(<<WsData/binary, DataBin/binary>>, <<>>) of
    {ok, stop} ->
      {stop, normal};
    {ok, <<>>, DataBinRes} ->
      {ok, NetState#egs_tcp_handler{ws_data = DataBinRes}};
    {ok, DataBin1, DataBinRes} ->
      decode_client_data(DataBin1, NetState#egs_tcp_handler{ws_data = DataBinRes})
  end.

%% socket进程退出
handle_terminate(Reason, NetState) ->
  %% TODO 增加用户退出逻辑
  ?LOG_INFO("player quit, reason: ~p state: ~p", [Reason, NetState]),
  ok.

%% socket的事件流
%% 可以对除了socket感兴趣之外的事件进行匹配处理

%% 玩家call调用
handle_event(?SOCKET_EVENT(?SOCKET_EVENT_CALL,  Request), #egs_tcp_handler{player = Player}=NetState) ->
  case ?TRY(egs_model:handle_main_msg(Request, call, Player)) of
    {ok, Reply, Player1} ->
      {ok, Reply, NetState#egs_tcp_handler{player = Player1}};

    {stop, Reason, Player1} ->
      {stop, Reason, NetState#egs_tcp_handler{player = Player1}};

    _What ->
      ?LOG_ERROR("call error ~w", [Request]),
      {ok, ?FAIL, NetState}
  end;

%% cast+info 调用
handle_event(?SOCKET_EVENT(?SOCKET_EVENT_CAST, Info), #egs_tcp_handler{player = Player}=NetState) ->
  case ?TRY(egs_model:handle_main_msg(Info, cast, Player)) of
    {ok, Reply, Player1} ->
      {ok, Reply, NetState#egs_tcp_handler{player = Player1}};

    {stop, Reason, Player1} ->
      {stop, Reason, NetState#egs_tcp_handler{player = Player1}};

    _What ->
      ?LOG_ERROR("cast error ~w", [_What]),
      {ok, ?FAIL, NetState}
  end;

handle_event(?SOCKET_EVENT(?SOCKET_EVENT_INFO, Info), #egs_tcp_handler{player = Player}=NetState) ->
  case ?TRY(egs_model:handle_main_msg(Info, info, Player)) of
    {ok, Reply, Player1} ->
      {ok, Reply, NetState#egs_tcp_handler{player = Player1}};

    {stop, Reason, Player1} ->
      {stop, Reason, NetState#egs_tcp_handler{player = Player1}};

    _What ->
      ?LOG_ERROR("info error ~w", [_What]),
      {ok, ?FAIL, NetState}
  end;

handle_event(_Evt, NetState) ->
  ?LOG_WARNING("unhandled event: ~p", [_Evt]),
  {ok, NetState}.

%% socket出错了，处理问题
handle_error(Reason, NetState) ->
  case Reason of
    ?SOCKET_ERROR_TCP_CLOSED ->
      %% 只有socket关闭才执行掉线
      %% TODO 考虑是否要处理掉线
      ?LOG_WARNING("player quit, reason: ~p", [Reason]),
      {stop, normal};

    _ ->
      {ok, NetState}
  end.


%% 数据处理
decode_client_data(<<Data/binary>>, State = #egs_tcp_handler{player = Player}) ->
  %% TODO 处理数据
  case egs_msg:decode(Data) of
    {?ERR_RET, _} ->
      %% 错误数据
      ?LOG_ERROR("decode error: ~p", [Data]),
      {ok, State#egs_tcp_handler{ws_data = <<>>}};

    #{data := Msg} ->
%%      ?LOG_INFO("recv msg: ~p", [Msg]),
      case ?TRY(egs_model:handle_c2s_msg(Msg, self(), Player)) of
        {?OK, Player1} ->
          %%
          %% 成功
          %% TODO 写入数据 - 扔数据到dump服务
          %%
          {ok, State#egs_tcp_handler{player = Player1, ws_data = <<>>}};

        {?ERR_RET, _} ->
          %%
          %% 处理失败
          %%
          ?LOG_ERROR("handle_c2s error: ~p", [Msg]),
          {ok, State#egs_tcp_handler{ws_data = <<>>}};

        {?OK, ?STOP, R} ->
          %%
          %% 退出
          %%
          {stop, R, State};

        Unknown ->
          %%
          %% 处理失败，未知错误
          %%
          ?LOG_ERROR("handle_c2s unknown: ~p", [Unknown]),
          {ok, State#egs_tcp_handler{ws_data = <<>>}}

      end
  end.