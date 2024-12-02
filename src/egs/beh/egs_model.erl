%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 消息模型
%%% @end
%%%-------------------------------------------------------------------
-module(egs_model).
-include("egs.hrl").

%%
%% 1.首先在egs.config中配置模块，然后通过build_handlers生成模块
%% 2.处理器是以1000做为步长，比如10000解析为：10，要调整，请修改宏EGS_PROTO_DIV
%% 3.使用了模块动态编译，所以模块名不能重复
%% 4.处理器等于协议处理。
%% 5.此框架为单进程模型，所以必须指定一个主处理器，然后实现egs_model
%%

-define(EGS_HANDLER_MGR, egs_model_mgr).
-define(EGS_PROTO_DIV, 1000).

%% API
-export([
  build_handlers/1,
  handle_c2s_msg/3,
  handle_main_msg/3,
  main/0,
  handlers/1
]).


%% 模块回调函数
-callback handle_call(Request ::term(), State ::term()) ->
  {ok, State ::term()} | {stop, Reason ::term(), NewState ::term()}.

-callback handle_cast(Request ::term(), State ::term()) ->
  {ok, State ::term()} | {stop, Reason ::term(), NewState ::term()}.

-callback handle_info(Info ::term(), State ::term()) ->
  {ok, State ::term()} | {stop, Reason ::term(), NewState ::term()}.

-callback handle_terminate(Reason ::term(), State ::term()) ->
  {ok, State ::term()} | {stop, Reason ::term(), NewState ::term()}.

%% 注册处理器
%% 注册到消息处理管理器，会动态编译模块，尽量在初始化处理好，频繁动态增加
%% 消息处理器规则：默认以1000作为补偿，即10000解析为：10,然后根据10查找handler
%% #{main=>mod_player, handlers=>#{10 => login_handler, 11=>player_handler}}
build_handlers(#{main:=Main, handlers:=Handlers}=M) when is_atom(Main), is_map(Handlers) ->
  Content = io_lib:format("handlers()-> ~w.\n", [M]),
  ?LOG_INFO("build handlers:~s", [Content]),
  egs_funs:compile_file(?EGS_HANDLER_MGR, Content),
  ok.

%% 主
%% 主处理器可以接受用户进程的其他消息
main() ->
  maps:get(main, ?EGS_HANDLER_MGR:handlers(), ?UNDEFINED).

%% 其他处理器
handlers(Proto) ->
  case maps:get(handlers, ?EGS_HANDLER_MGR:handlers(), ?UNDEFINED) of
    #{}=M -> maps:get(Proto, M, ?UNDEFINED);
    _ ->
      ?UNDEFINED
  end.

%% 处理协议消息
handle_c2s_msg(#{code := Code}=Msg, SocketPid, Player) ->
  case handlers(Code div ?EGS_PROTO_DIV) of
    ?UNDEFINED ->
      ?LOG_ERROR("handler not found: ~p", [Code]),
      {?ERR_RET, handler_not_found};
    Mod ->
      ?LOG_INFO("handler found: ~p", [Mod]),
      Mod:handle_c2s(Msg, SocketPid, Player)
  end.


%% 处理进程消息
handle_main_msg(Msg, MsgType, Player) ->
  case main() of
    ?UNDEFINED ->
      ?LOG_WARNING("main handler not found: ~p", [Msg]),
      {?ERR_RET, main_model_not_found};

    Mod ->
      ?LOG_INFO("main model found: ~p", [Mod]),
      if
        MsgType == call -> Mod:handle_call(Msg, Player);
        MsgType == cast -> Mod:handle_cast(Msg, Player);
        MsgType == info -> Mod:handle_info(Msg, Player);
        true -> Mod:handle_terminate(Msg, Player)
      end
  end.