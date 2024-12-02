%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 消息处理器
%%% @end
%%%-------------------------------------------------------------------
-module(egs_handler).
-include("egs.hrl").

%% 回调函数

-callback handle_c2s(Msg ::term(),SocketPid ::pid(), State ::term()) ->
  {ok, State ::term()} | {stop, Reason ::term(), NewState ::term()}.