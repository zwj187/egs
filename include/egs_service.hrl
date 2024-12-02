%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% service头文件
%%% @end
%%%-------------------------------------------------------------------
-ifndef(EGS_SERVICE_HRL).
-define(EGS_SERVICE_HRL, true).

%% ETS设计
-record(egs_service, {service, status = 0, worker_num = 0, start_time=0}).
-record(egs_service_worker, {service, num = 0, status = 0, worker_id=0}).

%% 状态
-define(SERVICE_STATE_NORMAL, normal).

%% 默认Unique
-define(DEF_SERVICE_UNIQUE, 10000).

%% 服务消息处理
-define(SERVICE_BROAD, service_broad).
-define(S2S_SERVICE_MSG(CALL, SERVICE, MSG), {service, CALL, SERVICE, MSG}).
-define(S2S_SERVICE_CALL(SERVICE, MSG), ?S2S_SERVICE_MSG(call, SERVICE, MSG)).
-define(S2S_SERVICE_CAST(SERVICE, MSG), ?S2S_SERVICE_MSG(cast, SERVICE, MSG)).



-endif.