%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 网络模块，TODO：websocket尚未使用ranch，看个人喜好调整。
%%% @end
%%%-------------------------------------------------------------------
-module(egs_net).
-include("egs.hrl").

%% API
-export([
  start_net/0,
  start_http/0,
  stop/0
]).

-define(HTTP_HANDLER, egs_http_handler).

%% @doc 启动监听器
start_net() ->
  NetConfig = egs_config:get_key(egs, net),
  ?LOG_INFO("Start Net: ~p~n", [NetConfig]),
  [Ip, Port, Count|_] = NetConfig,
  do_start_tcp(Ip, Port, Count).

-spec do_start_tcp(Ip::string(), Port ::integer(), ListenCount::integer()) -> ok.
do_start_tcp(Ip, Port, ListenCount) ->
  {ok, SupPID} = egs_tcp_sup:start(),
  ?LOG_INFO("Start Tcp Supervisor:~w", [SupPID]),
  ?LOG_INFO("Start Tcp Listener: ~s ~w count: ~w...", [Ip, Port, ListenCount]),
  egs_tcp_sup:start_listener(#{ip => Ip, port => Port, count => ListenCount}),
  ok.

%% 启动http 服务，基于cowboy
start_http() ->
  %%
  egs_funs:ensured_app([ranch,cowlib,cowboy]),
  case egs_config:get_key(egs, http) of
    [Ip, Port|_] ->
      ?LOG_INFO("Start Http: ~p,~p~n", [Ip, Port]),
      Dispatch = cowboy_router:compile([
        {'_', [{"/egs", egs_http_handler, []}]}
      ]),
      {ok, _} = cowboy:start_clear(egs_cb_http_handler,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
      );
    _ ->
      ?IGNORE
  end.


%% @doc 停止监听器
stop()->
  egs_tcp_listener:shutdown(),
  egs_tcp_sup:stop(),
  ok.