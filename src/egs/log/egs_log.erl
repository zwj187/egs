%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 日志服务
%%% @end
%%%-------------------------------------------------------------------
-module(egs_log).
-compile(export_all).
-include("egs.hrl").

%% TODO 使用lager
stop() ->
%%  ?WARNING("log stop", []),
  ok.


%% @doc 动态调整日志服务
start() ->
%%  ?PRINT("lager log start~n", []),
  lager:start(),
  %% 重置日志
  gen_event:delete_handler(lager_event, lager_file_backend, []),
%%  gen_event:delete_handler(lager_event, lager_console_backend, []),
  Args = egs_config:get_key(lager, handlers),
%%  ?PRINT("lager log start:~p", [Args]),
  gen_event:add_handler(lager_event, lager_file_backend, element(2, lists:keyfind(lager_file_backend, 1, Args))),
%%  gen_event:add_handler(lager_event, lager_console_backend, element(2, lists:keyfind(lager_console_backend, 1, Args))),
  ok.

