%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_instance).

%% API
-export([start/3, stop/0, register_instance_project_data/3, get_instance_project_data/1, get_instance_auth_token/1]).

%% We just use an empty map if there are no user supplied options
-define(DEFAULT_OPTIONS, #{}).

%% Prefix for instance process
-define(INSTANCE_PREFIX, "cfclient_instance_").

% CFClient top level supervisor reference
-define(TOP_LEVEL_SUP, cfclient_sup).

%% Prefixes for instance child supervisors
-define(POLL_SUP_PREFIX, "cfclient_instance_poll_supervisor_").
-define(METRICS_SUP_PREFIX, "cfclient_instance_metrics_supervisor_").

%% Prefixes for instance child workers
-define(FEATURE_CACHE_PREFIX, "cfclient_instance_feature_cache_worker").
-define(METRICS_EVALUATION_CACHE_PREFIX, "cfclient_instance_metrics_evaluation_cache_worker").
-define(METRICS_TARGET_CACHE_PREFIX, "cfclient_instance_metrics_target_cache_worker").

-spec start(InstanceName :: string(), ApiKey :: string(), Options :: map()) -> ok | not_ok.
start(ApiKey, InstanceName, Options) ->
  logger:info("Starting Client Instance: ~p", [InstanceName]),
  logger:info("Initializing Config"),
  Config = cfclient_config:parse_options(ApiKey, Options),
  ok = cfclient_config:register_instance_config(InstanceName, Config),
  case connect(ApiKey, InstanceName) of
    {ok, AuthToken} ->
      parse_project_data(InstanceName, AuthToken),
      start_instance(InstanceName);
    {not_ok, Error} ->
      {not_ok, Error}
  end.


-spec connect(ApiKey :: string(), InstanceName :: atom()) -> string() | {error, connect_failure, term()}.
connect(ApiKey, InstanceName) ->
  Opts = #{cfg => #{host => cfclient_config:get_instance_config_value(InstanceName, config_url)}, params => #{apiKey => list_to_binary(ApiKey)}},
  {_Status, ResponseBody, _Headers} = cfapi_client_api:authenticate(ctx:new(), Opts),
  case cfapi_client_api:authenticate(ctx:new(), Opts) of
    {ok, ResponseBody, _} ->
      AuthToken = maps:get('authToken', ResponseBody),
      {ok, AuthToken};
    {error, Response, _} ->
      logger:error("Error when authorising API Key. Error response: ~p~n", [Response]),
      {not_ok, Response}
  end.


%%-spec get_authtoken() -> string() | {error, authtoken_not_found, term()}.
%%get_authtoken() ->
%%  %% TODO - to support multiple Client instances, we'll need to parameterize the application name here.
%%  {ok, AuthToken} = application:get_env(cfclient, authtoken),
%%  binary_to_list(AuthToken).

-spec parse_project_data(InstanceName :: atom(), JwtToken :: string()) -> ok.
parse_project_data(InstanceName, JwtToken) ->
  JwtString = lists:nth(2, string:split(JwtToken, ".", all)),
  DecodedJwt = base64url:decode(JwtString),
  UnicodeJwt = unicode:characters_to_binary(DecodedJwt, utf8),
  Project = jsx:decode(string:trim(UnicodeJwt)),
  register_instance_project_data(InstanceName, Project, JwtToken).

%%-spec get_project_value(Key :: string()) -> string() | {error, key_not_found, term()}.
%%get_project_value(Key) ->
%%  cfclient_config:get_instance_project_value()
%%  binary_to_list(Value).

-spec stop() -> ok | {error, not_found, term()}.
stop() ->
  logger:debug("Stopping client"),
  stop_children(supervisor:which_children(?TOP_LEVEL_SUP)),
  %% TODO - to support multiple Client instances, we'll need to parameterize the application name here.
  unset_application_environment(application:get_all_env(cfclient)).

%% Internal functions
-spec start_instance(InstanceName :: atom()) -> ok.
start_instance(InstanceName) ->
  %% Get instance specific child references
  InstanceSupName = get_ref_from_instance(instance, InstanceName),
  FeatureCacheName = get_ref_from_instance(instance_cache, InstanceName),
  PollSupName = get_ref_from_instance(instance_poll_server, InstanceName),
  MetricsSupName = get_ref_from_instance(instance_metrics, InstanceName),

  %% Check if analytics is enabled and pass to the instance supervisor so it knows whether to start a metrics child or not.
  IsAnalyticsEnabled = cfclient_config:get_instance_config_value(InstanceName, analytics_enabled),

  %% Start instance specific supervisor
  {ok, _} = supervisor:start_child(?TOP_LEVEL_SUP, cfclient_instance_sup:child_spec(InstanceSupName, [
    #{instance_sup_name => InstanceSupName,
      feature_cache_name => FeatureCacheName,
      poll_sup_name => PollSupName,
      metrics_sup_name => MetricsSupName,
      is_analytics_enabled => IsAnalyticsEnabled}])),

  %% The module cfclient_poll_server_sup uses simple_one_for_one, so we start it dynamically.
  %% TODO - when streaming is implemented, we'll want to check if it is enabled and only start polling if streaming isn't enabled.
  %% We'll also want to code a fallback strategy to use polling if streaming fails, but that likely won't be coded in this module.
  {ok, _} = supervisor:start_child(PollSupName, [InstanceName]),
  ok.

register_instance_project_data(InstanceName, ProjectData, AuthToken) when is_map(ProjectData), is_binary(AuthToken) ->
  Instances = cfclient_config:get_all_instances(),
  ProjectMap = maps:put(project, ProjectData, maps:get(InstanceName, Instances)),
  AuthTokenMap = maps:put(auth_token, AuthToken, ProjectMap),
%%  NewInstance = maps:put(InstanceName, ProjectMap, Instances), WORKING
%%  NewInstances2 = maps:put(project, ProjectData, maps:get(InstanceName, InstanceProject)),
  NewInstances = Instances#{InstanceName => AuthTokenMap},
  application:set_env(cfclient, instances, NewInstances).

get_instance_project_data(InstanceName) when is_atom(InstanceName) ->
  Instances = cfclient_config:get_all_instances(),
  Instance = maps:get(InstanceName, Instances),
  Project = maps:get(project, Instance).

get_instance_auth_token(InstanceName) when is_atom(InstanceName) ->
  Instances = cfclient_config:get_all_instances(),
  Instance = maps:get(InstanceName, Instances),
  binary_to_list(maps:get(auth_token, Instance)).


get_ref_from_instance(instance, InstanceName) ->
  list_to_atom(?INSTANCE_PREFIX ++ atom_to_list(InstanceName));
get_ref_from_instance(instance_cache, InstanceName) ->
  list_to_atom(?FEATURE_CACHE_PREFIX ++ atom_to_list(InstanceName));
get_ref_from_instance(instance_poll_server, InstanceName) ->
  list_to_atom(?POLL_SUP_PREFIX ++ atom_to_list(InstanceName));
get_ref_from_instance(instance_metrics, InstanceName) ->
  list_to_atom(?METRICS_SUP_PREFIX ++ atom_to_list(InstanceName)).

-spec stop_children(Children :: list()) -> ok.
stop_children([{Id, _, _, _} | Tail]) ->
  supervisor:terminate_child(?TOP_LEVEL_SUP, Id),
  supervisor:delete_child(?TOP_LEVEL_SUP, Id),
  stop_children(Tail);
stop_children([]) -> ok.


-spec unset_application_environment(CfClientEnvironmentVariables :: list()) -> ok.
unset_application_environment([{Key, _} | Tail]) ->
  %% TODO - to support multiple Client instances, we'll need to parameterize the application name here.
  application:unset_env(cfclient, Key),
  unset_application_environment(Tail);
unset_application_environment([]) -> ok.



