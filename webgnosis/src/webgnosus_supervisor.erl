%%%-------------------------------------------------------------------
%%% twitter application supervisor
%%%-------------------------------------------------------------------

-module(tweeter_supervisor).
-behaviour(supervisor).

%% api
-export([
         start/0,
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
    %% Install tweeter error handler
     gen_event:swap_handler(alarm_handler, {alarm_handler, swap}, {tweeter_alarm_handler, tweet}),

    {ok, {{one_for_one, 3, 10},
	  [{tag1, 
	    {tweeter_interface, start_link, []},
	    permanent, 
	    10000, 
	    worker, 
	    [tweeter_interface]}
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
%% Function: start()
%% Description
%%--------------------------------------------------------------------
start() ->
    spawn(fun() ->
		  supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = [])
	  end).
%%--------------------------------------------------------------------
%% Function: start_in_shell_for_testing()
%% Description: start supervsior and spawn workers when requested
%% from erl shell.
%%--------------------------------------------------------------------
start_in_shell_for_testing() ->
    {ok, Pid} = supervisor:start_link({local,?MODULE}, ?MODULE, _Arg = []),
    unlink(Pid).




