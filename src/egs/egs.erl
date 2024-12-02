%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 框架EGS入口文件
%%% @end
%%%-------------------------------------------------------------------
-module(egs).
-include("egs.hrl").
%% API
-export([start/0, stop/0, go/0]).

start() ->
  %% config
  egs_config:load_egs(),
  %% 启动日志
  egs_log:start(),
  %% system信息
  egs_funs:system_info_metrics(),
  %% 启动缓存
  egs_cache:default(),
  %% 数据库
  egs_mysql:ensure_started(),
  %% 初始化service
  egs_service:build_service(),
  %% 启动net
  egs_net:start_net(),
  %% 启动http
  egs_net:start_http(),
  ok.

stop() ->
  %% 关闭服务
  egs_service:unregister_all_service(),
  %% 停止日志
  egs_log:stop(),
  ok.


go() ->
  ?PRINT("egs go", []),
  egs_funs:ensured_app([kernel, stdlib, crypto, sasl, inets, egs]),
  ok.