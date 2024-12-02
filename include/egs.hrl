%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 框架基础头文件
%%% @end
%%%-------------------------------------------------------------------
-ifndef(GF_H_H).
-define(GF_H_H, true).

%% App名字
-define(APP_NAME, egs).

%% 编译选项
-compile({parse_transform, lager_transform}).

%% 调试+日志
-define(PRINT(Format, Args), io:format(Format ++ "~n", Args)).

-define(LOG_INFO(Args), lager:info("Args:~p~n", [Args])).
-define(LOG_INFO(Format, Args), lager:info(Format, Args)).

-define(LOG_WARNING(Args), lager:warning("Args:~p~n", [Args])).
-define(LOG_WARNING(Format, Args), lager:warning(Format, Args)).

-define(LOG_ERROR(Args), lager:error("Args:~p~n", [Args])).
-define(LOG_ERROR(Format, Args), lager:error(Format, Args)).

%% 常用宏定
-define(TRUE, true).
-define(FALSE, false).
-define(IGNORE, ignore).
-define(SKIP, skip).
-define(OK, ok).
-define(UNDEFINED, undefined).
-define(NORMAL, normal).
-define(STOP, stop).
-define(KILL, kill).
-define(NOREPLY, noreply).
-define(REPLY, reply).
-define(YES, true).
-define(NO, false).
-define(YES_0, 0).
-define(NO_1, 1).
-define(NULL, ?UNDEFINED).
-define(FAIL, server_fail).
-define(ERR_RET, error).
-define(NONE, server_none).
-define(OK(Data), {?OK, Data}).
-define(APPLY(M, F, A), {apply, M, F, A}).
-define(UNICODE(BIN, CODE), unicode:characters_to_binary(BIN, CODE)).
-define(UNICODE(BIN), ?UNICODE(BIN, utf8)).

%% 条件判断
-define(IF(CON, Y, N), case (CON) of ?TRUE -> (Y);_ -> (N) end).

%% Try catch
-define(TRY(Expr), try (Expr) catch TReason:TERR -> ?LOG_ERROR("~w:~w", [TReason, TERR]),{?ERR_RET, TReason} end).

%% 断言
-define(ASSERT(AExpr, AMsg), case (AExpr) of ?TRUE -> ok; _ -> throw(AMsg) end).

%% 等待
-define(WAIT(T), receive after T -> ok end).
-define(SLEEP(T), ?WAIT(T)).

%% 根监控树
-define(ROOT_SUP, egs_sup).

%% sup(标准监控树)
-define(START_CHILD_SUP(Parent, ChildMod, InitFun, InitArgs), supervisor:start_child(Parent, {ChildMod, {ChildMod, InitFun, InitArgs}, transient, infinity, supervisor, [ChildMod]})).
%% worker(标准系统进程，自动重启)
-define(START_CHILD_WORKER(Parent, WorkerMod, InitFun, InitArgs, WorkID), supervisor:start_child(Parent, {WorkID, {WorkerMod, InitFun, InitArgs}, transient, 5000, worker, [WorkerMod]})).


%% ets常用宏
-define(ETS_PUBLIC_OPTS(TYPE, POS), [public, TYPE, named_table, {keypos, POS}, {write_concurrency, true}, {read_concurrency, true}]).
-define(NEW_ETS(NAME, TYPE, POS), ets:new(NAME, ?ETS_PUBLIC_OPTS(TYPE, POS))).
-define(NEW_ETS_SAFE(NAME, TYPE, POS1), case ets:info(NAME, size) of ?UNDEFINED -> ?NEW_ETS(NAME, TYPE, POS1);_ -> ok end).

%% 进程字典
-define(DICT_GET(DictKey), erlang:get({?MODULE, DictKey})).
-define(DICT_PUT(DictKey, Value), erlang:put({?MODULE, DictKey}, Value)).
-define(DICT_DEL(DictKey), erlang:erase({?MODULE, DictKey})).
-define(DICT_DEL_ALL(Fun), lists:foreach(fun({_, Data}) -> Fun(Data) end, erlang:erase())).

-define(DICT_MAP_UPDATE(Key, ValueKey, Value),
  case ?DICT_GET(Key) of
    ?UNDEFINED -> ?DICT_PUT(Key, #{ValueKey => Value});
    DictMap -> ?DICT_PUT(Key, DictMap#{ValueKey => Value}) end). %% TODO 暂时使用Map，可以考虑其他更优效率的算法







-endif.