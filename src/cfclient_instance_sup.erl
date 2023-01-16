%%%-------------------------------------------------------------------
%%% @doc CFClient instance supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_instance_sup).

-behaviour(supervisor).

%% API
-export([start_link/7, child_spec/2]).

%% Supervisor callbacks
-export([init/1]).

-define(INSTANCE_CHILD(Id, Module, Args, Type),
  #{
    id => Id,
    start => {Module, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => Type,
    modules => [Module]}).

-define(POLL_SUPERVISOR, cfclient_poll_server_sup).
-define(METRICS_SUPERVISOR, cfclient_metrics_sup).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link(InstanceSupName :: atom(),
    FeatureCacheName :: atom(),
    PollSupChildName :: atom(),
    MetricsSupChildName :: atom(),
    MetricsEvaluationCacheName :: atom(),
    MetricsTargetCacheName :: atom(),
    IsAnalyticsEnabled :: boolean()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
%% TODO - when streaming is implemented, we'll add its supervisor ref here
start_link(InstanceSupName, FeatureCacheName, PollSupChildName, MetricsSupChildName, MetricsEvaluationCacheName, MetricsTargetCacheName, IsAnalyticsEnabled) ->
  supervisor:start_link({local, InstanceSupName}, ?MODULE, [PollSupChildName, FeatureCacheName, MetricsSupChildName, MetricsEvaluationCacheName, MetricsTargetCacheName, IsAnalyticsEnabled]).

child_spec(Id, Args) ->
  #{
    id => Id,
    start => {?MODULE, start_link, Args},
    restart => permanent,
    shutdown => 5000, % shutdown timea
    type => supervisor,
    modules => [?MODULE]
  }.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init([PollSupChildName, FeatureCacheName, MetricsSupChildName, MetricsEvaluationCacheName, MetricsTargetCacheName, IsAnalyticsEnabled]) ->
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 5,
  SupFlags = #{
    strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},
  case IsAnalyticsEnabled of
    true ->
      {ok, {SupFlags, instance_children(FeatureCacheName, PollSupChildName, MetricsSupChildName, MetricsEvaluationCacheName, MetricsTargetCacheName, analytics_enabled)}};
    false ->
      {ok, {SupFlags, instance_children(FeatureCacheName, PollSupChildName, MetricsSupChildName, MetricsEvaluationCacheName, MetricsTargetCacheName, analytics_disabled)}}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

instance_children(FeatureCacheName, PollSupName, MetricsSupName, MetricsEvaluationCacheName, MetricsTargetCacheName, analytics_enabled) ->
  %% Feature Cache
  CacheWorkerChild = ?INSTANCE_CHILD(?METRICS_SUPERVISOR, ?METRICS_SUPERVISOR, [FeatureCacheName], worker),
  %% Polling
  PollSupChild = ?INSTANCE_CHILD(?POLL_SUPERVISOR, ?POLL_SUPERVISOR, [PollSupName], supervisor),
  %% Metrics
  MetricsSupChild = ?INSTANCE_CHILD(?METRICS_SUPERVISOR, ?METRICS_SUPERVISOR, [MetricsSupName, MetricsEvaluationCacheName, MetricsTargetCacheName], supervisor),
  [CacheWorkerChild, PollSupChild, MetricsSupChild];
%% Don't start a metrics supervisor if analytics is disabled
instance_children(FeatureCacheName, PollSupName,  _, _, _, analytics_disabled) ->
  %% Feature Cache
  CacheWorkerChild = ?INSTANCE_CHILD(?METRICS_SUPERVISOR, ?METRICS_SUPERVISOR, [FeatureCacheName], worker),
  %% Polling
  PollSupChild = ?INSTANCE_CHILD(?POLL_SUPERVISOR, ?POLL_SUPERVISOR, [PollSupName], supervisor),
  [CacheWorkerChild, PollSupChild].