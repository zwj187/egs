%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 主模块
%%% @end
%%%-------------------------------------------------------------------
-module(mod_main).
-behavior(egs_model).
-compile(export_all).

-include("egs.hrl").


%% 加载处理器
%% 处理器配置在egs.config中
load_handlers() ->
  Handlers = egs_config:get_key(egs, handlers),
  ?LOG_INFO("load handlers: ~p", [Handlers]),
  %%  build handlers
  egs_model:build_handlers(#{
    main => ?MODULE,
    handlers => maps:from_list(Handlers)
  }),
  ok.


%%
%% 模块处理函数
%%
handle_call(_Request, State) ->
  {?OK, ?OK, State}.

handle_cast(_Request, State) ->
  {?OK, State}.

handle_info(_Info, State) ->
  {?OK, State}.

handle_terminate(Reason, State) ->
  {stop, Reason, State}.