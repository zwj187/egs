%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% emysql 工具类函数
%%% @end
%%%-------------------------------------------------------------------
-module(egs_mysql_util).

%%
%% TODO N年前的函数，考虑重写
%%


-export([
  insert/2,
  insert_on/2,
  replace/2,
  update/3,
  select/3,
  select/6,
  select_union/6,
  delete/2
]).

%% 插入数据
-spec insert(TableName::atom(), FieldValueList::list()) -> binary().
insert(TableName, FieldValueList) ->
  Vsql = do_replace(egs_conv:to_list(FieldValueList), <<>>),
  Bin1 = list_to_binary(atom_to_list(TableName)),
  <<"insert into `", Bin1/binary, "` set ", Vsql/binary, ";">>.

%% 插入，如果存在则更新
-spec insert_on(TableName::atom(), FieldValueList::list()) -> binary().
insert_on(TableName, FieldValueList) ->
  Vsql = do_replace(egs_conv:to_list(FieldValueList), <<>>),
  Vsql1 = do_replace_on(egs_conv:to_list(FieldValueList), <<>>),
  Bin1 = list_to_binary(atom_to_list(TableName)),
  <<"insert into `", Bin1/binary, "` set ", Vsql/binary, " ON DUPLICATE KEY UPDATE ", Vsql1/binary, ";">>.

%% 替换
-spec replace(TableName::atom(), FieldValueList::list()) -> binary().
replace(TableName, FieldValueList) ->
  Vsql = do_replace(egs_conv:to_list(FieldValueList), <<>>),
  Bin1 = list_to_binary(atom_to_list(TableName)),
  <<"replace into `", Bin1/binary, "` set ", Vsql/binary, ";">>.


do_replace_on([], Expr) -> Expr;
do_replace_on([{Field, _Val}], Expr) ->
  FieldBin = list_to_binary(atom_to_list(Field)),
  <<Expr/binary, "`", FieldBin/binary, "` = values(`", FieldBin/binary, "`)">>;

do_replace_on([{Field, _Val} | T], Expr) ->
  FieldBin = list_to_binary(atom_to_list(Field)),
  do_replace_on(T, <<Expr/binary, "`", FieldBin/binary, "` = values(`", FieldBin/binary, "`)" ",">>).



do_replace([], Expr) ->
  Expr;

do_replace([{Field, Val}], Expr) when is_binary(Val) orelse is_list(Val) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = emysql_util:encode(Val),%%re:replace(Val, "'", "''", [global, {return, binary}]),
  <<Expr/binary, "`", Bin1/binary, "` = ", Bin2/binary>>;

do_replace([{Field, Val} | T], Expr) when is_binary(Val) orelse is_list(Val) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = emysql_util:encode(Val),%%re:replace(Val, "'", "''", [global, {return, binary}]),
  do_replace(T, <<Expr/binary, "`", Bin1/binary, "` = ", Bin2/binary, ",">>);

do_replace([{Field, Val}], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(egs_conv:to_list(Val)),
  <<Expr/binary, "`", Bin1/binary, "` = ", Bin2/binary>>;

do_replace([{Field, Val} | T], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(egs_conv:to_list(Val)),
  do_replace(T, <<Expr/binary, "`", Bin1/binary, "` = ", Bin2/binary, ",">>).


%% 更新
-spec update(TableName::atom(), FieldValueList::list(), WhereList::list()) -> binary().
update(TableName, FieldValueList, WhereList) ->
  Vsql = do_replace(egs_conv:to_list(FieldValueList), <<>>),
  Bin1 = list_to_binary(atom_to_list(TableName)),
  Bin2 = get_where_sql(egs_conv:to_list(WhereList), <<>>),
  <<"update `", Bin1/binary, "` set ", Vsql/binary, " ", Bin2/binary, ";">>.

%% 查询
-spec select(TableName::atom(), Fields::list(), WhereList::list()) -> binary().
select(TableName, Fields, WhereList) ->
  select(TableName, Fields, WhereList, [], [], []).

%% 查询
-spec select(TableName::atom(), Fields::list(), WhereList::list(), OrderList::list(), GroupByList::list(), Limit::list()) -> binary().
select(TableName, Fields, WhereList, OrderList, GroupByList, Limit) ->
  BinTableName = list_to_binary(atom_to_list(TableName)),
  BinFields = case Fields of
                "count(1)" ->
                  <<"count(1)">>;
                <<"*">> ->
                  <<"*">>;
                "*" -> <<"*">>;
                [all] -> <<"*">>;
                [max, Value] ->
                  do_max(Value);
                _ ->
                  do_select(Fields, <<>>)
              end,
  BinWhere = get_where_sql(egs_conv:to_list(WhereList), <<>>),
  BinOrder = get_order_sql(egs_conv:to_list(OrderList), <<>>),
  BinGroupBy = do_group_by(GroupByList),
  BinLimit = do_limit(Limit),
  <<"select ", BinFields/binary, " from `", BinTableName/binary, "` ", BinWhere/binary, " ", BinOrder/binary, " ", BinGroupBy/binary, " ", BinLimit/binary, ";">>.


select_union([_ | _] = DBList, TableName, Fields, WhereList, OrderList, Limit) ->
  %%%
  BinFields = case Fields of
                "count(1)" ->
                  <<"count(1)">>;
                <<"*">> ->
                  <<"*">>;
                "*" -> <<"*">>;
                [all] -> <<"*">>;
                [max, Value] ->
                  do_max(Value);
                _ ->
                  do_select(Fields, <<>>)
              end,
  BinWhere = get_where_sql(egs_conv:to_list(WhereList), <<>>),
  BinOrder = get_order_sql(egs_conv:to_list(OrderList), <<>>),
  BinLimit = do_limit(Limit),
  BinSelect = <<"select ", BinFields/binary, " from `", "#table", "` ", BinWhere/binary, " ", BinOrder/binary, " ", BinLimit/binary>>,
  do_select_union(DBList, TableName, <<"">>, <<>>, BinSelect).


do_select_union([], _, _, BinAcc, _) -> BinAcc;
do_select_union([DB | LT], Table, Split, BinAcc, BinSelect) ->
  %%
  BinTableName = list_to_binary(lists:concat([DB, ".", Table])),
  BinSelect1 = binary:replace(BinSelect, <<"#table">>, BinTableName),
  BinAcc1 = <<BinAcc/binary, Split/binary, BinSelect1/binary>>,
  do_select_union(LT, Table, <<"union">>, BinAcc1, BinSelect).



do_select([], Expr) ->
  Expr;

do_select([Fields], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Fields)),
  <<Expr/binary, Bin1/binary>>;

do_select([Fields | T], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Fields)),
  do_select(T, <<Expr/binary, Bin1/binary, ",">>).

do_max(Value) ->
  Bin1 = list_to_binary(atom_to_list(Value)),
  <<"max(", Bin1/binary, ")">>.

do_limit(Limit) ->
  case Limit of
    [Num] ->
      Bin1 = list_to_binary(egs_conv:to_list(Num)),
      <<"limit ", Bin1/binary>>;
    [Num1, Num2] ->
      Bin2 = list_to_binary(egs_conv:to_list(Num1)),
      Bin3 = list_to_binary(egs_conv:to_list(Num2)),
      <<"limit ", Bin2/binary, ",", Bin3/binary>>;
    [] -> <<>>
  end.

do_group_by([]) ->
  <<>>;
do_group_by(List) ->
  Bin = egs_conv:to_binary(string:join(List, ",")),
  <<"group by ", Bin/binary>>.

delete(TableName, WhereList) ->
  Bin1 = list_to_binary(atom_to_list(TableName)),
  Bin2 = get_where_sql(WhereList, <<>>),
  <<"delete from `", Bin1/binary, "` ", Bin2/binary, ";">>.


get_where_sql([], Expr) ->
  Expr;

get_where_sql([{Field, Val}], Expr) when is_binary(Val) orelse is_list(Val) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = emysql_util:encode(Val),%%re:replace(Val, "'", "''", [global, {return, binary}]),
  <<"where ", Expr/binary, "`", Bin1/binary, "` = ", Bin2/binary>>;

get_where_sql([{Field, Val} | T], Expr) when is_binary(Val) orelse is_list(Val) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = emysql_util:encode(Val),%%re:replace(Val, "'", "''", [global, {return, binary}]),
  get_where_sql(T, <<Expr/binary, "`", Bin1/binary, "` = ", Bin2/binary, " and ">>);

get_where_sql([{Field, Val}], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(egs_conv:to_list(Val)),
  <<"where ", Expr/binary, "`", Bin1/binary, "` = ", Bin2/binary>>;

get_where_sql([{Field, Val} | T], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(egs_conv:to_list(Val)),
  get_where_sql(T, <<Expr/binary, "`", Bin1/binary, "` = ", Bin2/binary, " and ">>);

get_where_sql([{Field, Operator, Val}], Expr) when is_binary(Val) orelse is_list(Val) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = emysql_util:encode(Val),%%re:replace(Val, "'", "''", [global, {return, binary}]),
  Bin3 = re:replace(Operator, "", "", [global, {return, binary}]),
  <<"where ", Expr/binary, "`", Bin1/binary, "`", Bin3/binary, Bin2/binary>>;

get_where_sql([{Field, Operator, Val} | T], Expr) when is_binary(Val) orelse is_list(Val) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = emysql_util:encode(Val),%%re:replace(Val, "'", "''", [global, {return, binary}]),
  Bin3 = re:replace(Operator, "", "", [global, {return, binary}]),
  get_where_sql(T, <<Expr/binary, "`", Bin1/binary, "`", Bin3/binary, Bin2/binary, " and ">>);

get_where_sql([{Field, Operator, Val}], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(egs_conv:to_list(Val)),
  Bin3 = re:replace(Operator, "", "", [global, {return, binary}]),
  <<"where ", Expr/binary, "`", Bin1/binary, "`", Bin3/binary, Bin2/binary>>;

get_where_sql([{Field, Operator, Val} | T], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(egs_conv:to_list(Val)),
  Bin3 = re:replace(Operator, "", "", [global, {return, binary}]),
  get_where_sql(T, <<Expr/binary, "`", Bin1/binary, "`", Bin3/binary, Bin2/binary, " and ">>);

get_where_sql([{Field, Operator, Val, OrAnd}], Expr) when is_binary(Val) orelse is_list(Val) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = emysql_util:encode(Val),%%re:replace(Val, "'", "''", [global, {return, binary}]),
  Bin3 = re:replace(OrAnd, "", "", [global, {return, binary}]),
  Bin4 = re:replace(Operator, "", "", [global, {return, binary}]),
  <<"where ", Expr/binary, "`", Bin1/binary, "`", Bin4/binary, Bin2/binary, Bin3/binary>>;

get_where_sql([{Field, Operator, Val, OrAnd} | T], Expr) when is_binary(Val) orelse is_list(Val) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = emysql_util:encode(Val),%%re:replace(Val, "'", "''", [global, {return, binary}]),
  Bin3 = re:replace(OrAnd, "", "", [global, {return, binary}]),
  Bin4 = re:replace(Operator, "", "", [global, {return, binary}]),
  get_where_sql(T, <<Expr/binary, "`", Bin1/binary, "`", Bin4/binary, Bin2/binary, Bin3/binary, " ">>);

get_where_sql([{Field, Operator, Val, OrAnd}], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(egs_conv:to_list(Val)),
  Bin3 = re:replace(OrAnd, "", "", [global, {return, binary}]),
  Bin4 = re:replace(Operator, "", "", [global, {return, binary}]),
  <<"where ", Expr/binary, "`", Bin1/binary, "`", Bin4/binary, "'", Bin2/binary, "' ", Bin3/binary>>;

get_where_sql([{Field, Operator, Val, OrAnd} | T], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(egs_conv:to_list(Val)),
  Bin3 = re:replace(OrAnd, "", "", [global, {return, binary}]),
  Bin4 = re:replace(Operator, "", "", [global, {return, binary}]),
  get_where_sql(T, <<Expr/binary, "`", Bin1/binary, "`", Bin4/binary, "'", Bin2/binary, "' ", Bin3/binary, " ">>).


get_order_sql([], Expr) ->
  Expr;

get_order_sql([{Field}], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  <<"order by ", Expr/binary, "`", Bin1/binary, "`">>;

get_order_sql([{Field} | T], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  get_order_sql(T, <<Expr/binary, "`", Bin1/binary, "`,">>);

get_order_sql([{Field, Order}], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(atom_to_list(Order)),
  <<"order by ", Expr/binary, "`", Bin1/binary, "` ", Bin2/binary>>;

get_order_sql([{Field, Order} | T], Expr) ->
  Bin1 = list_to_binary(atom_to_list(Field)),
  Bin2 = list_to_binary(atom_to_list(Order)),
  get_order_sql(T, <<Expr/binary, "`", Bin1/binary, "` ", Bin2/binary, ",">>).