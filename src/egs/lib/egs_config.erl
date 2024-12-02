%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 配置文件
%%% @end
%%%-------------------------------------------------------------------
-module(egs_config).
-include("egs.hrl").

%% API
-export([
  load_egs/0,
  read_config/1,
  get_key/2,
  get_key/1
]).

-define(EGS_CONFIG_MGR, egs_config_mgr).
-define(BASE_CONFIG_FILE, "egs.config").
-define(BASE_CONFIG_CONTENT,[]).
-define(CHECK_DIRS, ["./configs/","../configs/","./"]).

%% 加载EGS.CONFIG配置
%% 请保证文件放置在./configs/目录下
load_egs() ->
  case read_config(?BASE_CONFIG_FILE) of
    {ok, [Data]} ->
      compile_config(Data);
    _ ->?IGNORE
  end,
  ok.


%% 重新加载为AsName
compile_config(ConfigData) ->
  Content = io_lib:format("get(K) -> proplists:get_value(K, ~p).\n", [ConfigData]),
  egs_funs:compile_file(?EGS_CONFIG_MGR, Content).

%% key find faster
get_key(K) ->
  ?EGS_CONFIG_MGR:get(K).

%% 获取固定配置
get_key(Tag, Key) ->
  element(2, lists:keyfind(Key, 1, get_key(Tag))).


%% 读写文件
read_config(File) ->
  case check_file_exists(?CHECK_DIRS, File) of
    File1 when is_list(File1) ->
      file:consult(File1);
    _ ->
      ?LOG_ERROR("Open Config File:~s Error,not Found!!", [File]),
      not_found
  end.

%% 检测文件
check_file_exists([], File) ->
  ?IF(filelib:is_file(File), File, ?UNDEFINED);
check_file_exists([Dir | DL], File) ->
  ?IF(filelib:is_file(File),
    File,
    check_file_exists(DL, filename:join(Dir, File))
  ).

