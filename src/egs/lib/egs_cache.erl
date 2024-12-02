%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 简单实现了基于ETS的缓存模块 TODO 待增加expire功能
%%% @end
%%%-------------------------------------------------------------------
-module(egs_cache).
-include("egs.hrl").

%% API
-export([
  default/0,
  get_cache/1,
  get_cache/2,
  put_cache/2,
  put_cache/3,
  clear_cache/1,
  show_cache/0,
  new_cache/1
]).

-record(egs_cache, {key, value, expire=0}).

default()->
  ?LOG_INFO("start default cache..."),
  new_cache().

%% @doc 创建缓存
new_cache() ->
  new_cache(?MODULE).
new_cache(CacheName) ->
  ?NEW_ETS_SAFE(CacheName, set, #egs_cache.key).

%% @doc 获取缓存
get_cache(Key) ->
  get_cache(?MODULE, Key).
get_cache(CacheName, Key) ->
  ?LOG_INFO("get cache:~w", [Key]),
  ets:lookup(CacheName, Key).

%% @doc 添加缓存
put_cache(Key, Value) ->
  put_cache(?MODULE, Key, Value).
put_cache(CacheName, Key, Value) ->
  ?LOG_INFO("put cache:~w, ~w", [Key, Value]),
  ets:insert(CacheName, #egs_cache{key = Key, value = Value}).

%% @doc 清空缓存
clear_cache(CacheName) ->
  ?LOG_INFO("clear cache:~w", [CacheName]),
  ets:delete_all_objects(CacheName).

%% @doc 显示缓存
show_cache() ->
  ets:tab2list(?MODULE).