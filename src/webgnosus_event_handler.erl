%%%-------------------------------------------------------------------
%%% webgnosus alarm handler
%%%-------------------------------------------------------------------
-module(webgnosus_event_handler).

%% gen_event callbacks
-export([
         init/1, 
         handle_event/2, 
         handle_call/2, 
	 handle_info/2, 
         terminate/2,
         code_change/3
        ]).

%%====================================================================
%% gen_event callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Initializes server
%%--------------------------------------------------------------------
init(Args) ->
    io:format("webgnosus_alarm_handler init:~p~n",[Args]),
    {ok, 0}.

%%--------------------------------------------------------------------
%% Function: handle_event(Msg, N) -> {ok, N}
%% Description: handle events
%%--------------------------------------------------------------------
%% info messages
%%--------------------------------------------------------------------
handle_event(X, N) ->
    io:format("JUNK ~p~n",[X]),
    {ok, N+1}.
        
%%--------------------------------------------------------------------
%% Function: handle_call(_Request, N) -> {ok, N,  N}
%% Description: call back on server requestes
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

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
%%====================================================================
%% API
%%====================================================================

%%====================================================================
%%% Internal functions
%%====================================================================
