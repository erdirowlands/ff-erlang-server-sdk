%%%-------------------------------------------------------------------
%%% @doc CFClient metrics supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_metrics_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(METRICS_CHILD(Id, Module, Args, Type),
  #{
    id => Id,
    start => {Module, start_link, Args},
    restart => permanent,
    shutdown => 5000,
    type => Type,
    modules => [Module]}).

%% Server module
-define(METRICS_SERVER_MODULE, cfclient_metrics_server).

%% Cache worker module
-define(LRU_MODULE, lru).

%% Cache args
-define(CACHE_SIZE, 32000000).

%% Worker children references
-define(METRICS_SERVER_PREFIX, "cfclient_metrics_server_").
-define(METRICS_EVALUATION_CACHE_PREFIX, "cfclient_metrics_evaluation_cache_").
-define(METRICS_TARGET_CACHE_PREFIX, "cfclient_metrics_target_cache_").

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link(MetricsSupName :: atom(), InstanceName :: atom()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
%% TODO - when streaming is implemented, we'll add its supervisor ref here
start_link(MetricsSupName, InstanceName) ->
  supervisor:start_link({local, MetricsSupName}, ?MODULE, [InstanceName]).

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
init([InstanceName]) ->
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 5,
  SupFlags = #{
    %% Using rest for one which should allow us to preserve metrics if the metrics server dies as it is started last.
    %% But, if metrics cache dies then we'll lose all state for that interval and the poll server will
    %% correctly be started last (so it won't try to write to non-existent caches).
    strategy => rest_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},
  {ok, {SupFlags, metrics_children(InstanceName)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% TODO - this generates a child spec. I think for ID (first arg in METRICS_CHILD) we can safely just use the name
%% of the module (even for multi-instance usecase). It's when the children are started up that the instance specific
%% name is used (which is what we do).
metrics_children(InstanceName) ->
  {EvaluationCacheName, TargetCacheName, ServerName} = get_refs_from_instance(InstanceName),

  MetricsEvaluationCacheChild = ?METRICS_CHILD(EvaluationCacheName, ?LRU_MODULE, [[{local, EvaluationCacheName}], ?CACHE_SIZE, []], worker),
  MetricsTargetCacheChild = ?METRICS_CHILD(TargetCacheName, ?LRU_MODULE, [{local, TargetCacheName}, ?CACHE_SIZE, []], worker),
  MetricsServerChild = ?METRICS_CHILD(ServerName, ?METRICS_SERVER_MODULE, [ServerName, InstanceName], worker),
  [MetricsEvaluationCacheChild, MetricsTargetCacheChild, MetricsServerChild].

get_refs_from_instance(InstanceName) ->
  EvaluationCacheName = list_to_atom(?METRICS_EVALUATION_CACHE_PREFIX ++ atom_to_list(InstanceName)),
  TargetCacheName = list_to_atom(?METRICS_TARGET_CACHE_PREFIX ++ atom_to_list(InstanceName)),
  ServerName = list_to_atom(?METRICS_SERVER_PREFIX ++ atom_to_list(InstanceName)),
  {EvaluationCacheName, TargetCacheName, ServerName}.