%%%-------------------------------------------------------------------
%%% @doc Metrics supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_metrics_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% The metrics server worker to supervise
-define(METRICS_SERVER, cfclient_metrics_server).

%%%===================================================================
%%% API functions
%%%===================================================================


%% @doc Starts the supervisor
-spec(start_link(MetricsSupName :: atom()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MetricsSupName) ->
  supervisor:start_link({local, MetricsSupName}, ?MODULE, [?METRICS_SERVER]).

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
init([]) ->
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{
    strategy => simple_one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  %% TODO - make sure that when start_child/2 is called in the instance module, that the INSTANCE NAME is provided!!
  ChildSpec = #{
    %% `id` key is ignored if provided in a simple_one_for_one strategy so don't provide it
    start => {?METRICS_SERVER, start_link, []}, %% The args list is empty here, but when start_child/2 is called this list will be appended with the required args
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?METRICS_SERVER]},

  {ok, {SupFlags, [ChildSpec]}}.
