%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 登录模块
%%% @end
%%%-------------------------------------------------------------------
-module(login_handler).
-behavior(egs_handler).
-include("egs.hrl").
-include("err_code.hrl").

%% API
-export([
  handle_c2s/3
]).


handle_c2s(#{}=Msg, SocketPid, Player) ->
  ?PRINT("login: ~p", [Msg]),
  proto_msg:s2c_msg(SocketPid, ?ERR_SUCCESS, #{code => 1, name => <<"zwj187">>, age => 18}),
  {?OK, Player};

handle_c2s(_Msg, _SocketPid, State) ->
  {?OK, State}.
