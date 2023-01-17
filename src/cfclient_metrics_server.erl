%%%-------------------------------------------------------------------
%%% @doc

%%% @end
-module(cfclient_metrics_server).

-behaviour(gen_server).

-export([start_link/2, enqueue_metrics/4, set_evaluation_cache_pid/1, set_target_cache_pid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-include("cfclient_metrics_attributes.hrl").

-record(cfclient_metrics_server_state, {analytics_push_interval, metrics_cache_pid, metric_target_cache_pid, instance_name}).

start_link(ServerName, MetricsEvaluationCacheName, MetricsTargetCacheName, InstanceName) ->
  gen_server:start_link({local, ServerName}, ?MODULE, [MetricsEvaluationCacheName, MetricsTargetCacheName, InstanceName], []).

init([MetricsEvaluationCacheName, MetricsTargetCacheName, InstanceName]) ->
  AnalyticsPushInterval = cfclient_config:get_instance_config_value(InstanceName, analytics_push_interval),
  ok = store_evaluation_cache_name(),
  ok = store_target_cache_name(),
  State = #cfclient_metrics_server_state{analytics_push_interval = AnalyticsPushInterval, metrics_cache_pid = MetricsEvaluationCacheName, metric_target_cache_pid = MetricsTargetCacheName, instance_name = InstanceName},
  metrics_interval(AnalyticsPushInterval, MetricsEvaluationCacheName, MetricsTargetCacheName, InstanceName),
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State = #cfclient_metrics_server_state{analytics_push_interval = AnalyticsPushInterval, metrics_cache_pid = MetricsCachePID, metric_target_cache_pid = MetricTargetCachePID, instance_name = InstanceName}) ->
  metrics_interval(AnalyticsPushInterval, MetricsCachePID, MetricTargetCachePID, InstanceName),
  {noreply, State}.

metrics_interval(AnalyticsPushInterval, MetricsEvaluationCacheName, MetricsTargetCacheName, InstanceName) ->
  logger:info("Gathering and sending analytics with interval : ~p seconds", [AnalyticsPushInterval / 1000]),
  MetricsData = create_metrics_data(lru:keys(MetricsEvaluationCacheName), MetricsEvaluationCacheName, os:system_time(millisecond), []),
  MetricTargetData = create_metric_target_data(lru:keys(MetricsTargetCacheName), MetricsTargetCacheName, []),
  case post_metrics(MetricsData, MetricTargetData, InstanceName) of
    {ok, Response} ->
      logger:info("Successfully posted metric to ff-server: ~p~n: ", [Response]),
      reset_metrics_cache(MetricsEvaluationCacheName),
      reset_metric_target_cache(MetricsTargetCacheName);
    noop ->
      logger:info("No metrics to post for this Analytics interval"),
      noop;
    {not_ok, Response} ->
      logger:error("Error recieved from ff-server when posting metrics: ~p~n", [Response]),
      not_ok
  end,
  erlang:send_after(AnalyticsPushInterval, self(), trigger).

%% Don't sent a request to the API if no metrics gathered this interval
post_metrics([], [], _) ->
  noop;
post_metrics(MetricsData, MetricTargetData, InstanceName) ->
  ProjectData = cfclient_instance:get_instance_project_data(InstanceName),
  Environment = maps:get(environment, ProjectData),
  ClusterID = maps:get(clusterIdentifier, ProjectData),
  ClusterMap = #{cluster => ClusterID},
  AuthToken = cfclient_instance:get_instance_auth_token(InstanceName),
  RequestConfig = #{cfg => #{auth => #{'BearerAuth' => <<"Bearer ", AuthToken/binary>>}, host => cfclient_config:get_instance_config_value(InstanceName, events_url)}, params => #{metricsData => MetricsData, targetData => MetricTargetData}},
  case cfapi_metrics_api:post_metrics(ctx:new(), ClusterMap, Environment, RequestConfig) of
    {ok, Response, _} ->
      {ok, Response};
    {error, Response, _} ->
      {not_ok, Response}
  end.

enqueue_metrics(FlagIdentifier, Target, VariationIdentifier, VariationValue) ->
  set_to_metrics_cache(FlagIdentifier, Target, VariationIdentifier, VariationValue, evaluation_cache_name_to_pid()),
  set_to_metric_target_cache(Target, target_cache_name_to_pid()).

-spec create_metrics_data(MetricsCacheKeys :: list(), MetricsCachePID :: pid(), Timestamp :: integer(), Accu :: list()) -> list().
create_metrics_data([UniqueEvaluation | Tail], MetricsCachePID, Timestamp, Accu) ->
  %% Each key is the unique evaluation mapped to its evaluation occurrence count and target
  {Count, UniqueEvaluationTarget} = lru:get(MetricsCachePID, UniqueEvaluation),
  Metric = create_metric(UniqueEvaluation, UniqueEvaluationTarget, Count, Timestamp),
  create_metrics_data(Tail, MetricsCachePID, Timestamp, [Metric | Accu]);
create_metrics_data([], _, _, Accu) ->
  Accu.

%% TODO - we are passing in the target here, but so far only using the Global target per ff-server requirements.
%% however we will want to add an option to the config to disable that global config and use the actual target.
%% So for the moment the UniqueEvaluationTarget is unreferenced.
create_metric(UniqueEvaluation, UniqueEvaluationTarget, Count, TimeStamp) ->
  MetricAttributes = [
    #{
      key => ?FEATURE_IDENTIFIER_ATTRIBUTE,
      value => maps:get(feature_name, UniqueEvaluation)
    },
    #{
      key => ?FEATURE_NAME_ATTRIBUTE,
      value => maps:get(feature_name, UniqueEvaluation)
    },
    #{
      key => ?TARGET_ATTRIBUTE,
      value => ?TARGET_GLOBAL_IDENTIFIER
    },
    #{
      key => ?VARIATION_IDENTIFIER_ATTRIBUTE,
      value => maps:get(variation_identifier, UniqueEvaluation)
    },
    #{
      key => ?VARIATION_VALUE_ATTRIBUTE,
      value => maps:get(variation_value, UniqueEvaluation)
    },
    #{
      key => ?SDK_VERSION_ATTRIBUTE,
      value => ?SDK_VERSION_ATTRIBUTE_VALUE
    },
    #{
      key => ?SDK_TYPE_ATTRIBUTE,
      value => ?SDK_TYPE_ATTRIBUTE_VALUE
    },
    #{
      key => ?SDK_LANGUAGE_ATTRIBUTE,
      value => ?SDK_LANGUAGE_ATTRIBUTE_VALUE
    }
  ],

  #{
    timestamp => TimeStamp,
    count => Count,
    %% Camel case to honour the API.
    metricsType => ?METRICS_TYPE,
    attributes => MetricAttributes
  }.

create_metric_target_data([UniqueMetricsTargetKey | Tail], MetricsTargetCachePID, Accu) ->
  Target = lru:get(MetricsTargetCachePID, UniqueMetricsTargetKey),
  MetricTarget = create_metric_target(Target),
  create_metric_target_data(Tail, MetricsTargetCachePID, [MetricTarget | Accu]);
create_metric_target_data([], _, Accu) -> Accu.

create_metric_target(Target) ->
  F =
    fun(K, V, AccIn) ->
      Attribute = cfclient_evaluator:custom_attribute_to_binary(V),
      [#{key => K, value => Attribute} | AccIn]
    end,

  Attributes = case is_map_key(attributes, Target) of
                 true ->
                   maps:fold(F, [], maps:get(attributes, Target));
                 false ->
                   []
               end,

  Identifier = maps:get(identifier, Target),
  Name = maps:get(name, Target, Identifier),

  #{
    identifier => Identifier,
    name => Name,
    attributes => Attributes
  }.

value_to_binary(Value) when is_binary(Value) ->
  Value;
value_to_binary(Value) when is_atom(Value) ->
  atom_to_binary(Value);
value_to_binary(Value) when is_list(Value) ->
  list_to_binary(Value).

-spec set_to_metrics_cache(FlagIdentifier :: binary(), Target :: cfclient:target(), VariationIdentifier :: binary(), VariationValue :: binary(), MetricsCachePID :: pid()) -> atom().
set_to_metrics_cache(FlagIdentifier, Target, VariationIdentifier, VariationValue, MetricsCachePID) ->
  %% We want to capture the unique evaluations which are a combination of Flag and Variation (which includes the variation value and identifier)
  Evaluation = #{feature_name => FlagIdentifier, variation_identifier => VariationIdentifier, variation_value => VariationValue},
  %% In the cache, we map unique evaluations to two data points
  %% 1. A counter so we can count how many times it has occurred.
  %% 2. The target for the unique evaluation. At present, we use the so called Global Target when posting metrics to
  %% FF-server, but lets cache the actual target as in the future we want to enable real target posting for when we need to debug.
  case lru:contains_or_add(MetricsCachePID, Evaluation, {1, Target}) of
    {true, _} ->
      {Counter, CachedTarget} = lru:get(MetricsCachePID, Evaluation),
      lru:add(MetricsCachePID, Evaluation, {Counter + 1, CachedTarget});
    {false, _} ->
      noop
  end.

-spec set_to_metric_target_cache(Target :: cfclient:target(), MetricsTargetCachePID :: pid()) -> atom().
set_to_metric_target_cache(Target, MetricsTargetCachePID) ->
  %% Only store target if it's not anonymous.
  case value_to_binary(maps:get(anonymous, Target, <<"false">>)) of
    <<"false">> ->
      Identifier = maps:get(identifier, Target),
      %% We only want to store unique Targets. Targets are considered unique if they have different identifiers.
      %% We achieve this by mapping the identifier to the target it belongs to and checking if it exists before putting it in the cache.
      case lru:contains(MetricsTargetCachePID, Identifier) of
        true ->
          noop;
        false ->
          %% Key is identifier and value is the target itself
          lru:add(MetricsTargetCachePID, Identifier, Target)
      end;
    <<"true">> ->
      logger:debug("Not registering Target ~p~n for metrics because it is anonymous", [Target]),
      noop
  end.


-spec store_evaluation_cache_name(EvaluationCachePID :: pid(), InstanceName :: atom()) -> ok.
store_evaluation_cache_name(EvaluationCachePID, InstanceName) ->
  Instances = cfclient_config:get_all_instances(),
  NewInstanceMap = maps:put(metrics_evaluation_cache_pid, EvaluationCachePID, maps:get(InstanceName, Instances)),
  NewInstances = Instances#{InstanceName => NewInstanceMap},
  application:set_env(cfclient, instances, NewInstances).

-spec store_target_cache_name(TargetCachePID :: pid(), InstanceName :: atom()) -> ok.
store_target_cache_name(TargetCachePID, InstanceName) ->
  Instances = cfclient_config:get_all_instances(),
  NewInstanceMap = maps:put(metrics_target_cache_pid, TargetCachePID, maps:get(InstanceName, Instances)),
  NewInstances = Instances#{InstanceName => NewInstanceMap},
  application:set_env(cfclient, instances, NewInstances).

-spec get_metrics_evaluation_cache_pid(InstanceName :: atom()) -> any().
get_metrics_evaluation_cache_pid(InstanceName) ->
  Instances = cfclient_config:get_all_instances(),
  Instance = maps:get(InstanceName, Instances),
  PID = maps:get(metrics_evaluation_cache_pid, Instance).

-spec get_metrics_target_cache_pid(InstanceName :: atom()) -> any().
get_metrics_target_cache_pid(InstanceName) ->
  Instances = cfclient_config:get_all_instances(),
  Instance = maps:get(InstanceName, Instances),
  PID = maps:get(metrics_target_cache_pid, Instance).

-spec evaluation_cache_name_to_pid(InstanceName :: atom()) -> pid().
evaluation_cache_name_to_pid(InstanceName) ->
  {ok, MetricsCachePID} = application:get_env(cfclient, metrics_cache_pid),
  MetricsCachePID.

-spec target_cache_name_to_pid(InstanceName :: atom()) -> pid().
target_cache_name_to_pid(InstanceName) ->
  {ok, MetricsTargetCachePID} = application:get_env(cfclient, metrics_target_cache_pid),
  MetricsTargetCachePID.

-spec reset_metrics_cache(MetricsEvaluationCacheName :: pid()) -> ok.
reset_metrics_cache(MetricsEvaluationCacheName) ->
  lru:purge(whereis(MetricsEvaluationCacheName)).

-spec reset_metrics_cache(MetricsEvaluationCacheName :: pid()) -> ok.
reset_metric_target_cache(MetricsTargetCacheName) ->
  lru:purge(whereis(MetricsTargetCacheName)).

terminate(_Reason, _State = #cfclient_metrics_server_state{}) ->
  ok.