%%%-------------------------------------------------------------------
%%% @author bmjen
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(cfclient_poll_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-record(cfclient_poll_server_state, {poll_interval}).

start_link(InstanceName) ->
  logger:info("Starting poll server for instance: ~p", [InstanceName]),
  gen_server:start_link(?MODULE, [InstanceName], []).

init([InstanceName]) ->
  PollInterval = cfclient_config:get_instance_config_value(InstanceName, poll_interval),
  cfclient:retrieve_flags(),
  cfclient:retrieve_segments(),
  erlang:send_after(PollInterval, self(), trigger),
  State = #cfclient_poll_server_state{poll_interval = PollInterval},
  {ok, State}.

handle_call(_Request, _From, State = #cfclient_poll_server_state{}) ->
  {reply, ok, State}.

handle_cast(_Request, State = #cfclient_poll_server_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #cfclient_poll_server_state{poll_interval = PollInterval}) ->
  logger:info("Triggering poll server"),
  cfclient:retrieve_flags(),
  cfclient:retrieve_segments(),
  erlang:send_after(PollInterval, self(), trigger),
  {noreply, State}.

terminate(_Reason, _State = #cfclient_poll_server_state{}) ->
  ok.

code_change(_OldVsn, State = #cfclient_poll_server_state{}, _Extra) ->
  {ok, State}.
