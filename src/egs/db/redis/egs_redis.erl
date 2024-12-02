%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% REDIS工具类
%%% @end
%%%-------------------------------------------------------------------
-module(egs_redis).
-include("egs.hrl").

%%
%% TODO N年前的函数，考虑重写
%%

-export([
  add_pool/1,
  default_pool/1,

  query/2,
  query_noreply/2,
  pipe_query/2,
  transaction/2
]).

-export([
  delete/1,
  hash_put/2,
  hash_put_one/3,
  hash_get/2,
  hash_get_all/1,
  hash_del/2,
  hash_exists/2,
  hash_incr/3,
  hash_incr_by_float/3,

  list_push/2,
  list_pop/1,
  list_range/3,
  list_trim/3,
  list_len/1,

  set_add/2,
  set_del/2,
  set_exists/2,
  set_get_all/1,
  set_get_random/2,

  sorted_set_add/3,
  sorted_set_del/2,
  sorted_set_get_all/1,
  sorted_set_get_rank/2,
  sorted_set_get_range/3
]).

-define(REDIS_ENGINE, eredis).
-define(DB_REDIS_POOL_SIZE, 20).

%% 默认连接池
-spec default_pool(map()) -> ok.
default_pool(PoolConf) when is_map(PoolConf)->
  add_pool(PoolConf#{pool_size => ?DB_REDIS_POOL_SIZE}).

%% 根据Unique获取redis worker
get_worker_with_unique(Unique) ->
  egs_funs:get_worker_id_with_unique(?REDIS_ENGINE, ?DB_REDIS_POOL_SIZE, Unique).

%% 添加连接池
-spec add_pool(map()) -> ok.
add_pool( #{
  enable := OK,
  database:=DB,
  host:=Host,
  port:=Port,
  password:=Psw,
  pool_size:=Size,
  reconnect_sleep:=ReconnectSleep,
  timeout:=Timeout
}) ->

  ?IF(OK,
  lists:foreach(fun(ID) ->
    Name = egs_funs:make_worker_name(?REDIS_ENGINE, ID),
    {ok, PID} = ?START_CHILD_WORKER(?ROOT_SUP, ?REDIS_ENGINE, start_link, [Host, Port, DB, Psw, ReconnectSleep, Timeout], Name),
    erlang:register(Name, PID)
    end, lists:seq(1, Size)), ?OK),
  ok.

%%  API 接口

%% query
%% query(xx, ["LPUSH", "keylist", "value"])
query(Unique, CMD) ->
  Cli = get_worker_with_unique(Unique),
  case ?REDIS_ENGINE:q(Cli, CMD) of
    {ok, GET} ->
      GET;
    ERROR ->
      ?LOG_ERROR("query....~w ~w ~w", [Cli, CMD, ERROR]),
      ?ERR_RET
  end.

query_noreply(Unique, CMD) ->
  Cli = get_worker_with_unique(Unique),
  ?REDIS_ENGINE:q_noreply(Cli, CMD).


%% pipe
%% 建议多使用pipe，效率大大提高
pipe_query(Unique, CMD) ->
  Cli = get_worker_with_unique(Unique),
  case ?REDIS_ENGINE:qp(Cli, CMD) of
    [_ | _] = GET ->
      GET;
    ERROR ->
      ?LOG_ERROR("query....~w ~w ~w", [Cli, CMD, ERROR]),
      error
  end.


%% transaction
%% Fun must return commit
transaction(Unique, Fun) ->
  Cli = get_worker_with_unique(Unique),
  case ?REDIS_ENGINE:q(Cli, ["MULTI"]) of
    {ok, <<"OK">>} ->
      case ?TRY(Fun()) of
        commit ->
          {ok, _} = ?REDIS_ENGINE:q(Cli, ["EXEC"]);
        _ ->
          ?ERR_RET
      end
  end.


%% 处理返回值
make_return(B) when is_binary(B) ->
  make_ret_erl(B);
make_return(B) when is_list(B) ->
  [make_ret_erl(B1) || B1 <- B];
make_return(B) -> B.

make_ret_erl(B) -> make_ret_erl1(B).
make_ret_erl1(<<"OK">>) -> ok;
make_ret_erl1(?ERR_RET) -> ?ERR_RET;
make_ret_erl1({ok, R}) when is_integer(R) -> R;
make_ret_erl1({ok, R}) -> make_ret_erl(R);
make_ret_erl1(<<131, _, _/binary>> = Ret) -> erlang:binary_to_term(Ret);
make_ret_erl1(<<>>) -> [];
make_ret_erl1(Ret) when is_binary(Ret) -> egs_conv:bitstring_to_term(Ret);
make_ret_erl1(Ret) -> Ret.

%% 存储数据
make_put_data(Data) -> erlang:term_to_binary(Data).
make_put_data_list(DataList) ->lists:flatten(DataList).

%% 常用接口

delete(Key) ->
  Ret = query(get_worker_with_unique(Key), ["DEL", Key]),
  make_return(Ret).

%% Hash 操作
hash_put(Key, DataList) when is_list(DataList) ->
  Ret = query(get_worker_with_unique(Key), ["HMSET", Key | make_put_data_list(DataList)]),
  make_return(Ret).

hash_put_one(Key, DataKey, Data) ->
  Ret = query(get_worker_with_unique(Key), ["HSET", Key, DataKey, make_put_data(Data)]),
  make_return(Ret).

hash_get(Key, Field) when is_list(Field) ->
  Ret = query(get_worker_with_unique(Key), ["HMGET", Key | Field]),
  make_return(Ret);

hash_get(Key, Field) ->
  Ret = query(get_worker_with_unique(Key), ["HGET", Key, Field]),
  make_return(Ret).

hash_get_all(Key) ->
  Ret = query(get_worker_with_unique(Key), ["HGETALL", Key]),
  make_return(Ret).

hash_del(Key, Field) ->
  Ret = query(get_worker_with_unique(Key), ["HDEL", Key, Field]),
  make_return(Ret).

hash_exists(Key, Field) ->
  Ret = query(get_worker_with_unique(Key), ["HEXISTS", Key, Field]),
  make_return(Ret).

hash_incr(Key, Field, Num) ->
  Ret = query(get_worker_with_unique(Key), ["HINCRBY", Key, Field, Num]),
  make_return(Ret).

hash_incr_by_float(Key, Field, Num) ->
  Ret = query(get_worker_with_unique(Key), ["HINCRBYFLOAT", Key, Field, Num]),
  make_return(Ret).


%% List 操作
list_push(Key, Data) ->
  Ret = query(get_worker_with_unique(Key), ["RPUSH", Key, make_put_data(Data)]),
  make_return(Ret).

list_pop(Key) ->
  Ret = query(get_worker_with_unique(Key), ["LPOP", Key]),
  make_return(Ret).

list_range(Key, Start, End) ->
  Ret = query(get_worker_with_unique(Key), ["LRANGE", Key, Start, End]),
  make_return(Ret).

list_trim(Key, Start, End) ->
  Ret = query(get_worker_with_unique(Key), ["LTRIM", Key, Start, End]),
  make_return(Ret).

list_len(Key) ->
  Ret = query(get_worker_with_unique(Key), ["LLEN", Key]),
  make_return(Ret).


%% Set 操作
set_add(Key, Data) ->
  Ret = query(get_worker_with_unique(Key), ["SADD", Key, make_put_data(Data)]),
  make_return(Ret).

set_del(Key, Data) ->
  Ret = query(get_worker_with_unique(Key), ["SREM", Key, make_put_data(Data)]),
  make_return(Ret).

set_exists(Key, Data) ->
  Ret = query(get_worker_with_unique(Key), ["SISMEMBER", Key, make_put_data(Data)]),
  make_return(Ret).

set_get_all(Key) ->
  Ret = query(get_worker_with_unique(Key), ["SMEMBERS", Key]),
  make_return(Ret).

set_get_random(Key, Count) ->
  Ret = query(get_worker_with_unique(Key), ["SRANDMEMBER", Key, Count]),
  make_return(Ret).


%% Sorted Set 操作

sorted_set_add(Key, Data, Score) ->
  Ret = query(get_worker_with_unique(Key), ["ZADD", Key, Score, make_put_data(Data)]),
  make_return(Ret).

sorted_set_del(Key, Data) ->
  Ret = query(get_worker_with_unique(Key), ["ZREM", Key, make_put_data(Data)]),
  make_return(Ret).

 sorted_set_get_all(Key) ->
   Ret = query(get_worker_with_unique(Key), ["ZRANGE", Key, 0, -1, "WITHSCORES"]),
   make_return(Ret).

sorted_set_get_range(Key, Start, End) ->
  Ret = query(get_worker_with_unique(Key), ["ZRANGE", Key, Start, End, "WITHSCORES"]),
  make_return(Ret).

sorted_set_get_rank(Key, Data) ->
  Ret = query(get_worker_with_unique(Key), ["ZRANK", Key, make_put_data(Data)]),
  make_return(Ret).