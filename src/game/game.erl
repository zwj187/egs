%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 游戏逻辑进入逻辑
%%% @end
%%%-------------------------------------------------------------------
-module(game).
-include("egs.hrl").

%% API
-export([
  start/0,
  stop/0
]).


start() ->
  %% redis
  run_redis(),
  %% 数据库
  run_db(),
  %% 服务
  run_game_service(),
  %% 处理器
  run_handlers(),
  ok.


stop() ->
  ok.


%% 内部函数


%% 启动数据库
%% 使用默认池子
run_db() ->
  DbConf = egs_config:get_key(egs, mysql),
  ?LOG_INFO("run game database: ~w~n", [DbConf]),
  egs_mysql:default_pool(maps:from_list(DbConf)),
  ok.

%% 启动redis
run_redis() ->
  RedisConf = egs_config:get_key(egs, redis),
  ?LOG_INFO("run game redis: ~w~n", [RedisConf]),
  egs_redis:default_pool(maps:from_list(RedisConf)),
  ok.

%% @doc 开启游戏服务
run_game_service() ->
  GL = egs_service:all_service(),
  ?LOG_INFO("run game service: ~w~n",[GL]),
  [egs_service:register_service(S) || S <- GL].

%% @doc 初始化处理器
run_handlers()->
  mod_main:load_handlers(),
  ok.