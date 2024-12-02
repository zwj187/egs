%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 数据落地模块 TODO 待实现
%%% 1.上层返回落地数据，默认写回缓存并且定时批量多写落地数据到REDIS和MYSQL（可以指定落地方式）
%%% 2.实时写入REDIS TODO 考虑延迟写，也可以将redis换成DETS。
%%% 3.定时监测缓存数据，定时批量写入MYSQL
%%% 4.MYSQL - 默认60s写一次
%%% @end
%%%-------------------------------------------------------------------
-module(serv_dump).
-behavior(egs_service).
-include("egs.hrl").

%% API
-export([
  push_dump/3,

  handle_service_init/0,
  handle_service_enter/0,
  handle_service_terminate/0,
  handle_worker_init/1,
  handle_worker_call/3,
  handle_worker_cast/2,
  handle_worker_info/2,
  handle_worker_terminate/2
]).

%% dump msg
-define(DUMP_MSG(DumpName, Key, Data), {push_dump, DumpName, Key, Data}).
-define(DUMP_MYSQL, dump_mysql).

%% dump rule
-define(DUMP_MYSQL_TIMER, 60 * 1000).

%% 落地数据，异步方式
%% push_dump(DumpName, Key, Data)
push_dump(DumpName, Key, Data) ->
  egs_service:cast_service(?MODULE, ?DUMP_MSG(DumpName, Key, Data), 0),
  ok.

handle_service_init() ->
  ok.

handle_service_enter() ->
  ok.

handle_service_terminate() ->
  ok.

handle_worker_init(WorkerName) ->
  ?LOG_INFO("Dump Worker Init:~p ~p", [WorkerName, self()]),
  dump_timer(),
  ok.

handle_worker_call(_Request, _From, State) ->
  {noreply, State}.

%% dump msg
handle_worker_cast(?DUMP_MSG(DumpName, Key, Data), State) ->
  ?LOG_WARNING("Dump Cache Data:~p", [Data]),
  %% TODO 直接落地redis
%%  egs_redis:hash_put_one(DumpName, Key, Data),

  %% 延迟落地MYSQL
  ?DICT_MAP_UPDATE(DumpName, Key, Data),
  {noreply, State};

handle_worker_cast(_Request, State) ->
  {noreply, State}.

%% 批量落地数据
handle_worker_info(?DUMP_MYSQL, State) ->
  dump_mysql(),
  {noreply, State};

handle_worker_info(_Info, State) ->
  {noreply, State}.

handle_worker_terminate(_Reason, _State) ->
  %% 进程退出先落地数据
  dump_mysql(),
  ok.

%% 定时器
dump_timer() ->
  %% 启动定时器
  erlang:send_after(?DUMP_MYSQL_TIMER, self(), ?DUMP_MYSQL),
  ok.

%% 落地MYSQL
dump_mysql() ->
  %% TODO 落地数据具体实现
  ?TRY(?DICT_DEL_ALL(
    fun(DataMap) ->
      ?LOG_WARNING("Dump Data:~p", [DataMap]),
      ok
    end)),

  %% 重新启动定时器
  dump_timer(),
  ok.
