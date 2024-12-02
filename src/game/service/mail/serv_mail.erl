%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 邮件服务
%%% @end
%%%-------------------------------------------------------------------
-module(serv_mail).
-behavior(egs_service).
-include("egs.hrl").

%% API
-export([
  handle_service_init/0,
  handle_service_enter/0,
  handle_service_terminate/0,
  handle_worker_init/1,
  handle_worker_call/3,
  handle_worker_cast/2,
  handle_worker_info/2,
  handle_worker_terminate/2
]).


handle_service_init() ->
  ok.

handle_service_enter() ->
  ok.

handle_service_terminate() ->
  ok.

handle_worker_init(WorkerName) ->
  ?LOG_INFO("Mail Worker ~p ~p", [WorkerName, self()]),
  ok.

handle_worker_call(_Request, _From, State) ->
  {noreply, State}.

handle_worker_cast(_Request, State) ->
  {noreply, State}.

handle_worker_info(_Info, State) ->
  {noreply, State}.

handle_worker_terminate(_Reason, _State) ->
  ok.