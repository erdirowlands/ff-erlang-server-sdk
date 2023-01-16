%%%-------------------------------------------------------------------
%%% @doc Polling supervisor
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_poll_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Poll server module - polling is simple_one_for_one so we can use a constant reference across instances
-define(POLL_SERVER, cfclient_poll_server).

%%%===================================================================
%%% API functions
%%%===================================================================


%% @doc Starts the supervisor
-spec(start_link(PollSupName :: atom()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(PollSupName) ->
  supervisor:start_link({local, PollSupName}, ?MODULE, []).

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
    %% We use simple_one_for_one here which means the poll server child won't be started automatically when
    %% this supervisor starts. This allows us to dynamically start and stop the poll server when required.
    %% For example, if streaming is enabled we won't start the poll server. Or, if streaming fails, we can
    %% fallback to use polling while streaming recovers.
    strategy => simple_one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  ChildSpec = #{
    id => ?POLL_SERVER, %% `id` key is ignored if provided in a simple_one_for_one strategy
    start => {?POLL_SERVER, start_link, []}, %% The args list is empty here, but when start_child/2 is called this list will be appended with the required args for unique instances.
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [?POLL_SERVER]},

  {ok, {SupFlags, [ChildSpec]}}.
