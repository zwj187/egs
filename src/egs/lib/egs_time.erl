%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 时间模块 TODO 相关人员请增加所需的函数
%%% @end
%%%-------------------------------------------------------------------
-module(egs_time).
-compile(export_all).


%% @doc 当前时间戳-秒
now_sec()->
  erlang:system_time(second).

%% @doc 当前时间戳-毫秒
now_milli() ->
  erlang:system_time(millisecond).
