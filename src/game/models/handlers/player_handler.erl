%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 玩家模块
%%% @end
%%%-------------------------------------------------------------------
-module(player_handler).
-behavior(egs_handler).

-include("egs.hrl").
-include("err_code.hrl").


%% API
-export([
  handle_c2s/3
]).


handle_c2s(#{}=Msg, _SocketPid, Player) ->
  ?PRINT("player: ~p", [Msg]),
  {?OK, Player};

handle_c2s(_Msg, _SocketPid, State) ->
  {?OK, State}.