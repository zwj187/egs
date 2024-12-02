%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 消息处理器 TODO 初定协议用json，如果考虑用pB，再次封装此模块
%%% @end
%%%-------------------------------------------------------------------
-module(egs_msg).
-include("egs.hrl").

-define(JSON, jsx).

%% API
-export([
  decode/1,
  encode/1
]).

%% @doc 解压数据
decode(Bin) ->
  case ?TRY(?JSON:decode(Bin, [{return_maps, true}, {labels, attempt_atom}])) of
    Decode when is_map(Decode) -> Decode;
    D ->
      ?LOG_ERROR("Decode Error:~p", [D]),
      {?ERR_RET, bad_data}
  end .

%% @doc 压缩数据
encode(Bin) ->
  case ?TRY(?JSON:encode(Bin, [stream])) of
%%  case ?TRY(egs_conv:to_binary(egs_json:encode(Bin))) of
    Encode when is_binary(Encode) -> Encode;
    E ->
      ?LOG_ERROR("Encode Error:~p", [E]),
      {?ERR_RET, bad_data}
  end.
