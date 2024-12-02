%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 消息外部接口
%%% @end
%%%-------------------------------------------------------------------
-module(proto_msg).
-include("egs.hrl").

%% API
-export([
  s2c_msg/3,
  s2c_msg/4
]).

%% 客户端消息
%% s2c_msg(SocketPid, ErrCode, ErrMsg, ProtoData)
s2c_msg(SocketPid, ErrCode, ProtoData) ->
  s2c_msg(SocketPid, ErrCode, <<>>, ProtoData).

s2c_msg(SocketPid, ErrCode, ErrMsg, ProtoData) when is_bitstring(ErrMsg) ->
%%  BIN = egs_msg:encode(#{data =>ProtoData, err_code => ErrCode, err_msg => ErrMsg}),
%%  ?LOG_INFO("send s2c_msg: ~p", [BIN]),
  egs_tcp_socket:send_msg(SocketPid, egs_msg:encode(#{data =>ProtoData, err_code => ErrCode, err_msg => ErrMsg})).