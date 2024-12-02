%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 服务器行为
%%% @end
%%%-------------------------------------------------------------------
-module(egs_service).

-include("egs.hrl").
-include("egs_service.hrl").
-behavior(egs_server).

%% API
-export([handle_init/1, handle_call/3, handle_cast/2, handle_info/2, handle_terminate/2]).
-export([
  build_service/0,
  all_service/0,
  register_service/1,
  unregister_all_service/0
]).
-export([
  call_service/3,
  call_service/4,
  cast_service/3,
  cast_service/4,
  broadcast_service/3
]).

%%
%% 服务设计
%% 1. 服务器启动时，会自动注册所有服务
%% 2. 服务器停止时，会自动注销所有服务
%% 3.需要在egs.config中配置service模块，并且配置服务对应的进程数
%% 4.基于egs的监控根进程来启动。
%% 5.服务支持同步、异步、广播等操作，请求服务，同步请求时，需要指定唯一标志，来保障同步问题。
%% TODO：服务跨节点并未实现，属于未定义行为，请知释。
%% TODO：服务使用hash+unique的方式来确定worker，后期可以考虑优化。
%%

%% service回调
-callback handle_service_init() -> ok.
-callback handle_service_enter() -> ok.
-callback handle_service_terminate() -> ok.

%% worker回调
-callback handle_worker_init(WorkerName::atom()) ->
  ok.
-callback handle_worker_call(Request :: term(), From :: term(), State :: term()) ->
  {reply, Reply :: term(), NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback handle_worker_cast(Request :: term(), State :: term()) ->
  {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback handle_worker_info(Info :: term(), State :: term()) ->
  {noreply, NewState :: term()} | {stop, Reason :: term(), NewState :: term()}.

-callback handle_worker_terminate(Reason :: term(), State :: term()) ->
  ok.

%% 静态模块
-define(EGS_SERVICE_MODULE_MGR, egs_service_mgr).


%% 展示当前服务列表
all_service() ->
  ?EGS_SERVICE_MODULE_MGR:get_list().

%% 编译服务 FIXME 在egs配置好服务，启动自动静态化
build_service() ->
  Services = egs_config:get_key(egs, services), %% 获取服务列表
  Content = io_lib:format("get(K)-> proplists:get_value(K, get_list()).\nget_list() -> ~w .\n", [Services]),
  ?LOG_INFO("build config service:~s", [Content]),
  egs_funs:compile_file(?EGS_SERVICE_MODULE_MGR, Content).

%% @doc 注册服务
-spec register_service(Service :: {Mod::term(), WorkerNum::integer()}) -> ok.
register_service({Service, N}) when is_atom(Service), is_integer(N) ->
  ?ASSERT(egs_funs:is_behaviour(Service, ?MODULE) == ?TRUE, "bad ebgs_service behavior"),
  do_register_service(Service),
  ok.

%% 同步请求服务
%% Unique 如果需要的话，可以确保同步，比如：玩家的ID等) | MASTER
%% call_service(Service, Msg, Unique)
call_service(Service, Msg, Unique) ->
  call_service(Service, node(), Msg, Unique).

%% call_service(Service, ServiceNode, Msg, Unique)
call_service(Service, ServiceNode, Msg, Unique) ->
  ?TRY(do_service_send(Service, ServiceNode, call, Msg, Unique)).

%% 异步请求服务
%% cast_service(Service, Msg, Unique)
cast_service(Service, Msg, Unique) ->
  cast_service(Service, node(), Msg, Unique).

%% cast_service(Service, ServiceNode, Msg, Unique)
cast_service(Service, ServiceNode, Msg, Unique) ->
  ?TRY(do_service_send(Service, ServiceNode, cast, Msg, Unique)).

%% 广播服务
%% broadcast_service(Service, ServiceNode, Msg)
broadcast_service(Service, ServiceNode, Msg) ->
  ?TRY(do_service_send(Service, ServiceNode, cast, {?SERVICE_BROAD, Msg}, ?DEF_SERVICE_UNIQUE)).


%% 消息处理
%% TODO 暂时使用ETS来实现，后续可以使用静态编译
do_service_send(Service, ServiceNode, Call, Msg, Unique) ->
  case ?EGS_SERVICE_MODULE_MGR:get(Service) of
    Num when is_integer(Num) > 0 ->
      WorkerName = egs_funs:get_worker_id_with_unique(Service, Num, Unique),
      ?IF(Call == call,
        gen_server:call({WorkerName, ServiceNode}, ?S2S_SERVICE_CALL(Service, Msg)),
        gen_server:cast({WorkerName, ServiceNode}, ?S2S_SERVICE_CAST(Service, Msg))
      );
    _ ->
      ?LOG_ERROR("===service not register===:~p", [Service]),
      ?ERR_RET
  end.


%% 启动工作进程
start_service_worker(Sup, Service, WorkerNum) when WorkerNum > 0 ->
  fold_service(Service,
    fun(WorkerName) ->
      {ok, PID} = ?START_CHILD_WORKER(Sup, egs_server, start_link, [?MODULE, [Service, WorkerName], []], WorkerName),
      erlang:register(WorkerName, PID)
    end).


do_register_service(Service) when is_atom(Service) ->
  do_register_service({Service, ?EGS_SERVICE_MODULE_MGR:get(Service)});
do_register_service({Service, WorkerNum}) when is_atom(Service), is_integer(WorkerNum), WorkerNum > 0 ->
  %% 服务的初始化
  ?LOG_INFO("register service:~p", [Service]),
  ok = Service:handle_service_init(),
  start_service_worker(?ROOT_SUP, Service, WorkerNum),
  ok = Service:handle_service_enter().

%% 注销全部服务
unregister_all_service() ->
  ?LOG_INFO("unregister all service..."),
  lists:foreach(
    fun(ServiceData) ->
      unregister_service(element(1, ServiceData))
    end,
    %% 倒序
    lists:reverse(all_service())),
  ok.

%% 注销某服务
unregister_service(Service) ->
  fold_service(Service,
    fun(WorkerName) ->
      gen_server:call(WorkerName, stop),
      ?TRY(Service:handle_service_terminate())
    end),
  ok.

%% 循环执行
fold_service(Service, Fun) when is_function(Fun) ->
  Num = ?EGS_SERVICE_MODULE_MGR:get(Service),
  lists:foreach(
    fun(WorkerID) ->
      WorkerName = egs_funs:make_worker_name(Service, WorkerID),
      ?TRY(Fun(WorkerName))
    end, lists:seq(1, Num)),
  ok.


%% worker回调
handle_init([Service, WorkerName]) ->
  process_flag(trap_exit, true),
  erlang:process_flag(priority, high),
  %% worker初始化
  ?TRY(Service:handle_worker_init(WorkerName)),
  {ok, #egs_service_worker{service = Service, worker_id = WorkerName}}.

%% stop
handle_call(stop, _From, State) ->
  {stop, normal, State};


%% worker call
handle_call(?S2S_SERVICE_CALL(SERVICE, MSG), _From, #egs_service_worker{worker_id = WorkerID} = State) ->
  case ?TRY(SERVICE:handle_worker_call(MSG, WorkerID)) of
    {ok, Reply} ->
      {reply, Reply, State};
    _ ->
      {reply, ?FAIL, State}
  end;

handle_call(MSG, _From, #egs_service_worker{service = Service, worker_id = WorkerID} = State) ->
  case ?TRY(Service:handle_worker_call(MSG, WorkerID)) of
    {ok, Reply} ->
      {reply, Reply, State};
    _ ->
      {reply, ?FAIL, State}
  end.


%% worker cast

%% 广播
handle_cast(?S2S_SERVICE_CAST(SERVICE, {?SERVICE_BROAD, MSG}), State) ->
  fold_service(SERVICE, fun(WorkerName) -> WorkerName ! MSG end),
  {noreply, State};

handle_cast(?S2S_SERVICE_CAST(SERVICE, MSG), State) ->
  ?TRY(SERVICE:handle_worker_cast(MSG, State#egs_service_worker.worker_id)),
  {noreply, State};


handle_cast(MSG, #egs_service_worker{service = Service, worker_id = WorkerID} = State) ->
  ?TRY(Service:handle_worker_cast(MSG, WorkerID)),
  {noreply, State}.

%% worker info == cast
handle_info(MSG, #egs_service_worker{service = Service, worker_id = WorkerID} = State) ->
  ?TRY(Service:handle_worker_cast(MSG, WorkerID)),
  {noreply, State}.


handle_terminate(_Reason, #egs_service_worker{service = Service, worker_id = WorkerID}) ->
  ?TRY(Service:handle_worker_stop(WorkerID)),
  ok.