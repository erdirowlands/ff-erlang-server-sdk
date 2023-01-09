%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ffclient_stream_server).

-behavior(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(Args) ->
  logger:info("Streaming for changes"),
  cfapi_client_api:stream(),
  erlang:error(not_implemented).

handle_call(Request, From, State) ->
  erlang:error(not_implemented).

handle_cast(Request, State) ->
  erlang:error(not_implemented).

stream() ->
  CommonOpts = #{content_handlers => [gun_sse_h, gun_data_h]},
  {ok, Pid} = gun:open("config.ff.harness.io", 443, #{
%%    http_opts => CommonOpts
    http_opts => CommonOpts,
    transport => tls,
    protocols => [http]
  }),

  Ref = gun:get(Pid, "/api/1.0/stream?cluster=2", #{
    <<"accept">> => <<"text/event-stream">>,
    <<"API-Key">> => <<"">>,
    <<"Authorization">> => <<"Bearer ">> }
  ),

  {response, nofin, Code, RespHeaders} = gun:await(Pid, Ref).
%%  {sse, Event} = gun:await(Pid, Ref),
%%  #{
%%    last_event_id := LastEventID,
%%    event_type := <<"message">>,
%%    data := Data
%%  } = Event.