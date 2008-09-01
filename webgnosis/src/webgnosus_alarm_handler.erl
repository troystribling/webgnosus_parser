%%%-------------------------------------------------------------------
%%% twitter alarm handler
%%%-------------------------------------------------------------------
-module(tweeter_alarm_handler).
-behaviour(gen_event).

%% gen_event callbacks
-export([
         init/1, 
         handle_event/2, 
         handle_call/2, 
	 handle_info/2, 
         terminate/2
        ]).

%%====================================================================
%% gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Args) ->
    io:format("*** tweeter_alarm_handler init:~p~n",[Args]),
    {ok, 0}.

%%--------------------------------------------------------------------
%% Function: handle_event(Msg, N) -> {ok, N}
%% Description: handle events
%%--------------------------------------------------------------------
%% info messages
%%--------------------------------------------------------------------
handle_event({info_message, {}}, N) ->
    error_logger:info_msg("*** Tell the Engineer to turn on the fan~n"),
    {ok, N+1};
    
    
%%--------------------------------------------------------------------
%% Function: handle_call(_Request, N) -> {ok, N,  N}
%% Description: handle server calls
%%--------------------------------------------------------------------
handle_call(_Request, N) -> _Reply = N, {ok, N,  N}.

%%--------------------------------------------------------------------
%% Function: handle_info(_Info, N) -> {ok, N}
%% Description: handle info requests
%%--------------------------------------------------------------------
handle_info(_Info, N) -> {ok, N}.

%%--------------------------------------------------------------------
%% Function: terminate(_Reason, _N)   -> ok
%% Description: terminate event server
%%--------------------------------------------------------------------
terminate(_Reason, _N) -> ok.
