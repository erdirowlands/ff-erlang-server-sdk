%%%-------------------------------------------------------------------
%%% @doc CFClient instance supervisor
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_instance_sup).

-behaviour(supervisor).

%% API
-export([start_link/3]).

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
-spec(start_link(InstanceSupName :: atom(), PollSupChildName :: atom(), MetricsSupChildName :: atom()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
%% TODO - when streaming is implemented, we'll add its supervisor ref here
start_link(InstanceSupName, PollSupChildName, MetricsSupChildName) ->
  supervisor:start_link({local, InstanceSupName}, ?MODULE, [PollSupChildName, MetricsSupChildName]).

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
init([PollSupChildName, MetricsSupChildName, LRUCacheWorkerName]) ->
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 5,
  SupFlags = #{
    strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  {ok, {SupFlags, instance_children(PollSupChildName, MetricsSupChildName, LRUCacheWorkerName)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

instance_children(PollSupName, MetricsSupName, LRUCacheName) ->
  LRUWorkerChild = ?INSTANCE_CHILD(?METRICS_SUPERVISOR, ?METRICS_SUPERVISOR, [LRUCacheName], worker),
  PollSupChild = ?INSTANCE_CHILD(?POLL_SUPERVISOR, ?POLL_SUPERVISOR, [PollSupName], supervisor),
  MetricsSupChild = ?INSTANCE_CHILD(?METRICS_SUPERVISOR, ?METRICS_SUPERVISOR, [MetricsSupName], supervisor),
  [PollSupChild, MetricsSupChild, LRUWorkerChild].