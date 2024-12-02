%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Http模块
%%% @end
%%%-------------------------------------------------------------------
-module(egs_http_handler).
-include("egs.hrl").
-behaviour(cowboy_handler).

%%
%% TODO 基于cowboy来实现的Http服务
%% 下面增加对应的API回调
%%

%% API
-export([
  init/2
]).

init(Req, _Opts) ->
%%  ?PRINT("http init:~p", [Req]),
%%  TODO 增加Http消息处理
  Method = cowboy_req:method(Req),
  ?LOG_INFO("http recv:~p", [Method]),
  case Method of
    <<"POST">> ->
      {ok, _Body, Req2} = cowboy_req:read_body(Req),
      {ok, ReplyBody} = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        <<"POST GOT">>,
        Req2),
      {ok, ReplyBody, []};
    _ ->
      {ok, ReplyBody} = cowboy_req:reply(405,
        #{<<"content-type">> => <<"text/plain">>},
        <<"Bad Request">>,
        Req),
      {ok, ReplyBody, []}
  end.
