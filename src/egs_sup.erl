%%%-------------------------------------------------------------------
%% @doc egs top level supervisor.
%% 游戏服务监控树
%% @end
%%%-------------------------------------------------------------------

-module(egs_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/2, start_child/1]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(SupFlags, ChildSpecs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, #{sup_flags => SupFlags, child_specs => ChildSpecs}).

start_child(ChildSpec) ->
    supervisor:start_child(?MODULE, ChildSpec).

%% 默认是启动内部进程
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}};

init(#{sup_flags := SupFlags, child_specs := ChildSpecs}) ->
%%    SupFlags = #{
%%        strategy => one_for_one,
%%        intensity => 10,
%%        period => 10
%%    },
%%    ChildSpecs = #{
%%        id => egs_server,
%%        start => {egs_server, start, []},
%%        restart => permanent,
%%        shutdown => 5000,
%%        type => supervisor,
%%        modules => [egs_server]
%%    },
    {ok, {SupFlags, ChildSpecs}}.
