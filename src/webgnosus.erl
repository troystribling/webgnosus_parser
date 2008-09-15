%%%-------------------------------------------------------------------
%%% webgnosus analysis main module
%%%-------------------------------------------------------------------

-module(webgnosus).
-behaviour(application).

%% api
-export([
	 start/0,
	 stop/0,
         create_tables/0,
         delete_tables/0,
         clear_tables/0
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
%% Returns: {ok, Pid}         
%%          {ok, Pid, State}  
%%          {error, Reason}   
%% Description: start called by application:start()
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    webgnosus_events:message({started, ?MODULE, StartArgs}),
    inets:start(),
    mnesia:start(),
    mnesia:wait_for_tables([laconica_sites], 20000),
    webgnosus_supervisor:start_link(StartArgs).

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: ok 
%% Description: stop called by application:stop()
%%--------------------------------------------------------------------
stop(_State) ->
    inets:stop(),
    mnesia:stop(),
    webgnosus_events:message({stopped, ?MODULE}),
    ok.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/0
%% Returns: {ok, Pid}         
%%          {ok, Pid, State}  
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

%%--------------------------------------------------------------------
%% database API
%%--------------------------------------------------------------------
%% Func: create_tables/0
%% Returns: 
%% Description: create application database tables
%%--------------------------------------------------------------------
create_tables() ->
    mnesia:start(),
    laconica_site_model:create_table(),
    mnesia:stop(),
    ok.

%%--------------------------------------------------------------------
%% Func: delete_tables/0
%% Returns: 
%% Description: delete application database tables
%%--------------------------------------------------------------------
delete_tables() ->
   mnesia:start(),
   laconica_site_model:delete_table(),
   mnesia:stop(),
   ok.

%%--------------------------------------------------------------------
%% Func: reset_tables/0
%% Returns: 
%% Description: delete all rows in application database tables
%%--------------------------------------------------------------------
clear_tables() ->
    laconica_site_model:clear_tables(),
    ok.
