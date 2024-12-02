%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% MYSQL简单封装
%%% @end
%%%-------------------------------------------------------------------
-module(egs_mysql).
-include("egs.hrl").

%%
%% TODO N年前的函数，考虑重写
%%

%% API
-export([
  ensure_started/0,
  add_pool/1,
  default_pool/1
]).

-export([
  insert/2,
  insert/3,

  insert_on/2,
  insert_on/3,

  replace/2,
  replace/3,

  update/3,
  update/4,

  select_all/6,
  select_all/7,

  select_row/4,
  select_row/5,

  select_union/6,
  select_union/7,

  select_count/3,
  select_count/2,

  select_max/2,
  select_max/3,

  delete/2,
  delete/3,

  execute/1,
  execute/2,
  execute/3
]).

-define(DB_ENGINE, emysql).
-define(DB_POOL, egs_pool).

%% 确保启动
ensure_started() ->
  case application:start(?DB_ENGINE) of
    ok -> ok;
    {error, Reason} ->
      ?LOG_ERROR("mysql start failed: ~p", [Reason]),
      ?ERR_RET
  end.

%% 默认连接池
-spec default_pool(map()) -> ok.
default_pool(PoolConf) when is_map(PoolConf)->
  add_pool(PoolConf#{pool => ?DB_POOL}).

%% 添加连接池
-spec add_pool(map()) -> ok.
add_pool( #{
  enable := OK,
  pool:=Pool,
  database:=DB,
  host:=Host,
  port:=Port,
  user:=User,
  password:=Psw,
  encoding:=Coding,
  pool_size:=Size
}) ->
  %% add_pool(PoolId, Size, User, Password, Host, Port, Database, Encoding)
  ?IF(OK, ?DB_ENGINE:add_pool(Pool, Size, User, Psw, Host, Port, DB, Coding), ?OK),
  ok.




%%	@doc 成功返回ok   出错返回 Errmsg
%%  insert(TableName, FieldValueList)
%%  TableName = atom
%%  FieldValueList = [{Field1,Value1},{Field2,Value2},...]
insert(TableName, FieldValueList) ->
  insert(?DB_POOL, TableName, FieldValueList).

insert(PoolID, TableName, FieldValueList) ->
  SQL = egs_mysql_util:insert(TableName, FieldValueList),
  sql_result(PoolID, SQL).


%%	@doc 成功返回ok   出错返回 Errmsg
%%  replace(TableName, FieldValueList)
%%  TableName = atom
%%  FieldValueList = [{Field1,Value1},{Field2,Value2},...]
insert_on(TableName, FieldValueList) ->
  insert_on(?DB_POOL, TableName, FieldValueList).

insert_on(PoolID, TableName, FieldValueList) ->
  SQL = egs_mysql_util:insert_on(TableName, FieldValueList),
  sql_result(PoolID, SQL).


%%	@doc 成功返回ok   出错返回 Errmsg
%%  replace(TableName, FieldValueList)
%%  TableName = atom
%%  FieldValueList = [{Field1,Value1},{Field2,Value2},...]
replace(TableName, FieldValueList) ->
  replace(?DB_POOL, TableName, FieldValueList).

replace(PoolID, TableName, FieldValueList) ->
  SQL = egs_mysql_util:replace(TableName, FieldValueList),
  sql_result(PoolID, SQL).


%%	@doc 成功返回ok   出错返回 Errmsg
%%  update(TableName, FieldValueList,WhereList)
%%  TableName = atom
%%  FieldValueList = [{Field1,Value1},{Field2,Value2},...]
%%  WhereList = [{Field1,Value1},{Field2,Value2},...] |
%%              [{Field1,Operator1,Value1},{Field2,Operator2,Value2},...] |
%%              [{Field1,Operator1,Value1,Orand},{Field2,Operator2,Value2,Orand},...]
%%  Operator = "<" | ">" | "="...
%%  Orand = "or" | "and"
update(TableName, FieldValueList, WhereList) ->
  update(?DB_POOL, TableName, FieldValueList, WhereList).

update(PoolID, TableName, FieldValueList, WhereList) ->
  SQL = egs_mysql_util:update(TableName, FieldValueList, WhereList),
  sql_result(PoolID, SQL).


%%	@doc 成功返回List   出错返回 Errmsg
%%  select_all(PoolID, TableName, Fields, WhereList, OrderList, GroupByList, Limit)
select_all(TableName, Fields, WhereList, OrderList, GroupByList, Limit) ->
  select_all(?DB_POOL, TableName, Fields, WhereList, OrderList, GroupByList, Limit).

select_all(PoolID, TableName, Fields, WhereList, OrderList, GroupByList, Limit) ->
  SQL = egs_mysql_util:select(TableName, Fields, WhereList, OrderList, GroupByList, Limit),
  sql_result(PoolID, SQL).

%% select_union(PoolID, DBList, TableName, Fields, WhereList, OrderList, Limit)
select_union(DBList, TableName, Fields, WhereList, OrderList, Limit) ->
  select_union(?DB_POOL, DBList, TableName, Fields, WhereList, OrderList, Limit).
select_union(PoolID, DBList, TableName, Fields, WhereList, OrderList, Limit) ->
  SQL = egs_mysql_util:select_union(DBList, TableName, Fields, WhereList, OrderList, Limit),
  sql_result(PoolID, SQL).

%% select_row(PoolID, TableName, Fields, WhereList, OrderList)
select_row(TableName, Fields, WhereList, OrderList) ->
  select_row(TableName, Fields, WhereList, OrderList).

select_row(PoolID, TableName, Fields, WhereList, OrderList) ->
  SQL = egs_mysql_util:select(TableName, Fields, WhereList, OrderList, [], [1]),
  sql_result(PoolID, SQL).

%% select_count(PoolID, TableName, WhereList)
select_count(TableName, WhereList) ->
  select_count(TableName, WhereList).

select_count(PoolID, TableName, WhereList) ->
  SQL = egs_mysql_util:select(TableName, "count(1)", WhereList),
  sql_result(PoolID, SQL).

%% select_max(PoolID, TableName, Fields)
select_max(TableName, Fields) ->
  select_max(?DB_POOL, TableName, Fields).
select_max(PoolID, TableName, Fields) ->
  select_max(PoolID, TableName, Fields, []).
select_max(PoolID, TableName, [F], WhereList) ->
  select_max(PoolID, TableName, [max, F], WhereList);
select_max(PoolID, TableName, Fields, WhereList) ->
  %% Fields = [max,F] ; WhereList = [] or List
  SQL = egs_mysql_util:select(TableName, Fields, WhereList),
  sql_result(PoolID, SQL).


%%	成功返回ok   出错返回 Errmsg
delete(TableName, WhereList) ->
  delete(?DB_POOL, TableName, WhereList).

delete(PoolID, TableName, WhereList) ->
  SQL = egs_mysql_util:delete(TableName, WhereList),
  sql_result(PoolID, SQL).

%% 执行SQL
%% execute(PoolID, SQL)
execute(SQL) ->
  execute(?DB_POOL, SQL).

execute(PoolID, SQL) ->
  sql_result(PoolID, SQL).

%% execute(PoolID, SQL, TimeOUT)
execute(PoolID, SQL, TimeOUT) ->
  Ret = ?DB_ENGINE:execute(PoolID, SQL, TimeOUT),
  check_sql_result(Ret, SQL).

%% mysql result
%% sql_result(PoolID, SQL)
sql_result(PoolID, SQL) ->
  Ret = ?DB_ENGINE:execute(PoolID, SQL),
  check_sql_result(Ret, SQL).


check_sql_result(Ret, SQL) when is_tuple(Ret) ->
  check_sql_result1(Ret, SQL);
check_sql_result(Ret, SQL) when is_list(Ret) ->
  [check_sql_result1(R, SQL) || R <- Ret].

check_sql_result1(Ret, SQL) ->
  case Ret of
    {ok_packet, _, _, _, _, _, _} -> ok;
    {result_packet, _, _, Result, _} -> Result;
    {error_packet, _, _, _, ErrMsg0} ->
      ?LOG_ERROR("~s Execute Failed ==>> ~p", [SQL, ErrMsg0]),
      error;
    Err ->
      ?LOG_ERROR("~s Execute Failed ==>> ~p", [SQL, Err]),
      error
  end.