%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 包装gen_server行为
%%% @end
%%%-------------------------------------------------------------------
-module(egs_server).
-include("egs.hrl").
-behavior(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/1, start/2, start/3, start_link/3]).


%% 回调函数
-callback handle_init(InitArgs ::term()) ->
  {ok, State ::term()} | {stop, Reason ::term()}.

-callback handle_call(Request ::term(), From ::term(), State ::term()) ->
  {reply, Reply ::term(), NewState ::term()} | {stop, Reason ::term(), NewState ::term()}.

-callback handle_cast(Request ::term(), State ::term()) ->
  {noreply, NewState ::term()} | {stop, Reason ::term(), NewState ::term()}.

-callback handle_info(Info ::term(), State ::term()) ->
  {noreply, NewState ::term()} | {stop, Reason ::term(), NewState ::term()}.

-callback handle_terminate(Reason ::term(), State ::term()) ->
  ok.

-record(mod, {mod, state}).

start(Mod) ->
  start(Mod, [], []).
start(Mod, Args) ->
  start(Mod, Args, []).
start(Mod, Args, Options) ->
  gen_server:start(?MODULE, [Mod,Args], set_start_options(Options)).
start_link(Mod, Args, Options) ->
  gen_server:start_link(?MODULE, [Mod,Args], set_start_options(Options)).

init([Mod, Args]) ->
  ?LOG_INFO("start server:~p,~p", [Mod, Args]),
  ?ASSERT(egs_funs:is_behaviour(Mod, ?MODULE) == ?TRUE, "bad ebgs_server behavior"),
  process_flag(trap_exit, true),
  case ?TRY(Mod:handle_init(Args)) of
    {?OK, State}-> {?OK, #mod{mod=Mod, state=State}};
    {?STOP, Reason} -> {?STOP, Reason}
  end.

handle_call(Request, From, State) ->
  case ?TRY((State#mod.mod):handle_call(Request, From, State#mod.state)) of
    {reply, Reply, Data} ->
      {reply, Reply, State#mod{state = Data}};
    {noreply, Data} ->
      {noreply, State#mod{state = Data}};
    {stop, Reason, Data} ->
      {stop, Reason, State#mod{state = Data}};
    {stop, Reason} ->
      {stop, Reason, State};
    Reply ->
      {reply, Reply, State}
  end.

handle_cast(Request, State) ->
  case ?TRY((State#mod.mod):handle_cast(Request, State#mod.state)) of
    {noreply, Data} ->
      {noreply, State#mod{state = Data}};
    {stop, Reason, Data} ->
      {stop, Reason, State#mod{state = Data}};
    {stop, Reason} ->
      {stop, Reason, State};
    _ ->
      {noreply, State}
  end.

handle_info(Info, State) ->
  case ?TRY((State#mod.mod):handle_info(Info, State#mod.state)) of
    {noreply, Data} ->
      {noreply, State#mod{state = Data}};
    {stop, Reason, Data} ->
      {stop, Reason, State#mod{state = Data}};
    {stop, Reason} ->
      {stop, Reason, State};
    _ ->
      {noreply, State}
  end.

terminate(Reason, State) ->
  ?TRY((State#mod.mod):handle_terminate(Reason, State#mod.state)),
  ok.


%% 设置启动参数
set_start_options(Options) -> Options.