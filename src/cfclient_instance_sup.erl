%%%-------------------------------------------------------------------
%%% @doc CFClient instance supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_instance_sup).

-behaviour(supervisor).

%% API
-export([start_link/6, child_spec/2]).

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

%% Supervisor modules
-define(METRICS_SUP_MODULE, cfclient_metrics_sup).
-define(POLL_SUPERVISOR, cfclient_poll_server_sup).

%% Cache worker module
-define(FEATURE_CACHE_MODULE, lru).

%% Cache args
-define(FEATURE_CACHE_ARGS, {max_size, 32000000}).


%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link(InstanceSupName :: atom(),
    FeatureCacheName :: atom(),
    PollSupChildName :: atom(),
    MetricsSupChildName :: atom(),
    InstanceName :: atom(),
    IsAnalyticsEnabled :: boolean()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
%% TODO - when streaming is implemented, we'll add its supervisor ref here
start_link(InstanceSupName, FeatureCacheName, PollSupChildName, MetricsSupChildName, IsAnalyticsEnabled, InstanceName) ->
  supervisor:start_link({local, InstanceSupName}, ?MODULE, [InstanceName, FeatureCacheName, PollSupChildName, MetricsSupChildName, IsAnalyticsEnabled]).

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
init([InstanceName, FeatureCacheName, PollSupChildName, MetricsSupChildName, IsAnalyticsEnabled]) ->
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 5,
  SupFlags = #{
    strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},
  case IsAnalyticsEnabled of
    true ->
      {ok, {SupFlags, instance_children(InstanceName, FeatureCacheName, PollSupChildName, MetricsSupChildName, analytics_enabled)}};
    false ->
      {ok, {SupFlags, instance_children(InstanceName, FeatureCacheName, PollSupChildName, MetricsSupChildName, analytics_disabled)}}
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

instance_children(InstanceName, FeatureCacheName, PollSupName, MetricsSupName, analytics_enabled) ->
  CacheWorkerChild = ?INSTANCE_CHILD(FeatureCacheName, ?FEATURE_CACHE_MODULE, [FeatureCacheName], worker),
  PollSupChild = ?INSTANCE_CHILD(PollSupName, ?POLL_SUPERVISOR, [InstanceName], supervisor),
  MetricsSupChild = ?INSTANCE_CHILD(MetricsSupName, ?METRICS_SUP_MODULE, [MetricsSupName], supervisor),
  [CacheWorkerChild, PollSupChild, MetricsSupChild];
instance_children(InstanceName, FeatureCacheName, PollSupName, _, analytics_disabled) ->
  CacheWorkerChild = ?INSTANCE_CHILD(FeatureCacheName, ?METRICS_SUP_MODULE, [FeatureCacheName], worker),
  PollSupChild = ?INSTANCE_CHILD(PollSupName, ?POLL_SUPERVISOR, [InstanceName], supervisor),
  [CacheWorkerChild, PollSupChild].