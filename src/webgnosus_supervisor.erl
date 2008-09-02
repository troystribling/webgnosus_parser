%%%-------------------------------------------------------------------
%%% webgnosus application supervisor
%%%-------------------------------------------------------------------

-module(webgnosus_supervisor).
-behaviour(supervisor).

%% api
-export([
         start_in_shell_for_testing/0
        ]).

%% supervisor callbacks
-export([
         start_link/1, 
         init/1
        ]).

%%====================================================================
%% supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Initialize server
%%--------------------------------------------------------------------
init([]) ->
    %% Install alarm and event handler
     gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {webgnosus_alarm_handler, webgnosus}),

    {ok, {{one_for_one, 3, 10},
	  [{tag1, 
	    {laconica_interface, start_link, []},
	    permanent, 
	    10000, 
	    worker, 
	    [laconica_interface]}
	  ]}}.

%%--------------------------------------------------------------------
%% Function: start_link(Args) -> Result
%% Description: start server and spawn workers. called by application.
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, Args).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_in_shell_for_testing()
%% Description: start supervsior and spawn workers when requested
%% from erl shell.
%%--------------------------------------------------------------------
start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).




