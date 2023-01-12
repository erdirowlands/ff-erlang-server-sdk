%%%-------------------------------------------------------------------
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_instance).

%% API
-export([start/3, stop/0, register_instance_project_data/3, get_instance_project_data/1, get_instance_auth_token/1]).

-define(DEFAULT_OPTIONS, #{}).
-define(PARENTSUP, cfclient_sup).

%% Prefixes for client instances
-define(INSTANCE_PREFIX, "cfclient_instance_").
-define(POLL_SERVER_PREFIX, "cfclient_instance_poll_server_").
-define(METRICS_SERVER_PREFIX, "cfclient_instance_metrics_server_").



%% Child references
-define(POLL_SERVER_CHILD_REF, cfclient_poll_server).
-define(LRU_CACHE_CHILD_REF, cfclient_lru).
-define(METRICS_GEN_SERVER_CHILD_REF, cfclient_metrics_server).
-define(METRICS_CACHE_CHILD_REF, cfclient_metrics_server_lru).
-define(METRIC_TARGET_CACHE_CHILD_REF, cfclient_metrics_server_target_lru).

-spec start(InstanceName :: string(), ApiKey :: string(), Options :: map()) -> ok | not_ok.
start(ApiKey, InstanceName, Options) ->
  logger:info("Starting Client Instance: ~p", [InstanceName]),
  logger:info("Initializing Config"),
  Config = cfclient_config:parse_options(ApiKey, Options),
  ok = cfclient_config:register_instance_config(InstanceName, Config),
  case connect(ApiKey, InstanceName) of
    {ok, AuthToken} ->
      parse_project_data(InstanceName, AuthToken),
      start_children(InstanceName);
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
  stop_children(supervisor:which_children(?PARENTSUP)),
  %% TODO - to support multiple Client instances, we'll need to parameterize the application name here.
  unset_application_environment(application:get_all_env(cfclient)).

%% Internal functions
-spec start_children(InstanceName :: atom()) -> ok.
start_children(InstanceName) ->
  %% Start Feature/Group Cache
  {ok, CachePID} = supervisor:start_child(?PARENTSUP, {lru, {lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
  cfclient_cache_repository:set_pid(CachePID),
  case cfclient_config:get_instance_config_value(InstanceName, analytics_enabled) of
    %% If analytics are enabled then we need to start the metrics gen server along with two separate caches for metrics and metrics targets.
    true ->
      %% Start metrics and metrics target caches
      {ok, MetricsCachePID} = supervisor:start_child(?PARENTSUP, {?METRICS_CACHE_CHILD_REF, {lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
      cfclient_metrics_server:set_metrics_cache_pid(MetricsCachePID),
      {ok, MetricsTargetCachePID} = supervisor:start_child(?PARENTSUP, {?METRIC_TARGET_CACHE_CHILD_REF, {lru, start_link, [[{max_size, 32000000}]]}, permanent, 5000, worker, ['lru']}),
      cfclient_metrics_server:set_metrics_target_cache_pid(MetricsTargetCachePID),
      %% Start metrics gen server
      {ok, _} = supervisor:start_child(?PARENTSUP, {?METRICS_GEN_SERVER_CHILD_REF, {cfclient_metrics_server, start_link, []}, permanent, 5000, worker, ['cfclient_metrics_server']});
    false -> ok
  end,
  %% Start Poll Processor
  {ok, _} = supervisor:start_child(?PARENTSUP, {?POLL_SERVER_CHILD_REF, {cfclient_poll_server, start_link, []}, permanent, 5000, worker, ['cfclient_poll_server']}),
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
get_ref_from_instance(instance_poll_server, InstanceName) ->
  list_to_atom(?POLL_SERVER_PREFIX ++ atom_to_list(InstanceName));
get_ref_from_instance(instance_metrics_server, InstanceName) ->
  list_to_atom(?METRICS_SERVER_PREFIX ++ atom_to_list(InstanceName)).

-spec stop_children(Children :: list()) -> ok.
stop_children([{Id, _, _, _} | Tail]) ->
  supervisor:terminate_child(?PARENTSUP, Id),
  supervisor:delete_child(?PARENTSUP, Id),
  stop_children(Tail);
stop_children([]) -> ok.


-spec unset_application_environment(CfClientEnvironmentVariables :: list()) -> ok.
unset_application_environment([{Key, _} | Tail]) ->
  %% TODO - to support multiple Client instances, we'll need to parameterize the application name here.
  application:unset_env(cfclient, Key),
  unset_application_environment(Tail);
unset_application_environment([]) -> ok.



