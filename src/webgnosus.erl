%%%-------------------------------------------------------------------
%%% webgnosus analysis main module
%%%-------------------------------------------------------------------

-module(webgnosus).
-behaviour(application).

%% api
-export([
	 start/0,
	 stop/0
        ]).

%% application callbacks
-export([
	 start/2,
	 stop/1
        ]).

%%====================================================================
%% application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%% Description: start called by application:start()
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    webgnosus_events:message({started, ?MODULE}),
    webgnosus_supervisor:start_link(StartArgs).

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%% Description: stop called by application:stop()
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/0
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}   
%% Description: start application
%%--------------------------------------------------------------------
start() ->
    application:start(webgnosus).

%%--------------------------------------------------------------------
%% Func: stop/0
%% Returns: any 
%% Description: stop application
%%--------------------------------------------------------------------
stop() ->
    application:stop(webgnosus).
