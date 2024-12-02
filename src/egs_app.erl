%%%-------------------------------------------------------------------
%% @doc egs public API
%% app入口文件
%% @end
%%%-------------------------------------------------------------------

-module(egs_app).
-include("egs.hrl").
-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    %% single node
    {ok, Sup} = egs_sup:start_link(),
    %% frm
    egs:start(),
    %% game
    game:start(),
    {ok, Sup}.

stop(_State) ->
    %% game
    game:stop(),
    %% frm
    egs:stop(),
    ok.
