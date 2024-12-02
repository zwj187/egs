%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 框架工具类函数
%%% @end
%%%-------------------------------------------------------------------
-module(egs_funs).
-include("egs.hrl").
-compile(export_all).

%% @doc 启动APP
start_app(#{app := APP, modules :=Mods}=Args) ->
  AppStr = "{application, ~w,
  [
    {description, \"erlang server application!!\"},
    {vsn, \"1.0.0\"},
    {modules, ~w},
    {registered, ~w},
    {applications, ~w},
    {mod, ~w},
    {env, ~w}
  ]}.",
  Regs = maps:get(regs, Args, []),
  Deps = maps:get(deps, Args, []),
  Mod = maps:get(mod, Args, []),
  Env = maps:get(env, Args, []),
  AppStr1 = io_lib:format(AppStr, [APP, Mods, Regs, Deps, Mod, Env]),
  ok = file:write_file(lists:concat([APP, ".app"]), AppStr1, [write, sync]),
  ok = application:start(APP),
  ok.

ensured_app(APP_LIST) when is_list(APP_LIST) ->
  lists:foreach(fun ensured_app/1, APP_LIST),
  ok;
ensured_app(APP) when is_atom(APP) ->
  ?PRINT("start app: ~p", [APP]),
  case application:start(APP) of
    ok -> ok;
    {error, {already_started, _}} -> ok;
    Err ->
      ?PRINT("start app error:~p", [Err])
  end,
  ok.


%% @doc 检测是否实现了某个接口
-spec is_behaviour(atom(), atom()) -> boolean().
is_behaviour(Mod, Behaviour) ->
  code:ensure_loaded(Mod),
  BL = proplists:get_value(behaviour,proplists:get_value(attributes, Mod:module_info()),[]),
  BL1 = proplists:get_value(behavior,proplists:get_value(attributes, Mod:module_info()),[]),
  lists:member(Behaviour, BL++BL1).


%% @doc 当前堆栈
stacktrace() ->
  element(2, erlang:process_info(self(), current_stacktrace)).


%% @doc 输出必要的system信息
system_info_metrics() ->
  PrintList = [
    cpu_topology,
    schedulers,
    max_heap_size,
    min_bin_vheap_size,
    min_bin_vheap_size,
    c_compiler_used,
    system_version,
    creation,
    logical_processors,
    dist_buf_busy_limit,
    ets_limit,
    atom_limit,
    process_limit,
    port_limit
  ],
  lists:foreach(
    fun(Print) ->
      ?LOG_INFO("===~s:~p~n", [Print, catch erlang:system_info(Print)])
    end, PrintList),
  ok.


%% @doc 判断是否有效进程
is_proc_alive(Proc) when is_pid(Proc) -> ?IF(catch erlang:is_process_alive(Proc), ?TRUE, ?FALSE);
is_proc_alive(?UNDEFINED) -> ?FALSE;
is_proc_alive(Proc) when is_atom(Proc) -> is_proc_alive(whereis(Proc));
is_proc_alive({Proc, Node}) when Node == node() -> is_proc_alive(Proc); %% 跨节点
is_proc_alive({Proc, Node}) when is_atom(Node) ->
  case rpc:call(Node, ?MODULE, is_proc_alive, [Proc]) of
    R when is_boolean(R) -> R;
    _ -> ?FALSE
  end;
is_proc_alive(_) -> ?FALSE.

%% @doc 获取配置
get_app_config(Key) ->
  get_app_config(Key, ?UNDEFINED).
get_app_config(Key, Def) ->
  case application:get_env(?APP_NAME, Key) of
    {ok, Val} -> Val;
    _ -> Def
  end.

%% 生成worker id
make_worker_name(Service, ID) ->
  egs_conv:to_atom(lists:concat([Service,"_worker_", ID])).

%% 根据Unique获取固定worker id
get_worker_id_with_unique(Service, WorkerMaxNum, Unique) when WorkerMaxNum > 0->
  WorkerID = erlang:phash2({Service, Unique}, WorkerMaxNum),
  make_worker_name(Service, WorkerID).

%% 动态编译文件
compile_file(Module, Content) ->
  Str = lists:flatten([io_lib:format("-module(~p).\n-compile(export_all).\n\n", [Module]), Content]),
%%  io:format("compile string:~n~s~n", [Str]),
  {Module, Code} = dynamic_compile:from_string(Str),
  case code:load_binary(Module, lists:concat([Module, ".erl"]), Code) of
    {module, Module} -> ?TRUE;
    Err ->
      ?PRINT("~ncompile error:~p", [Err]),
      ?FALSE
  end.
