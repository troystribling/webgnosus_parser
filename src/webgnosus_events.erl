%%%-------------------------------------------------------------------
%%% webgnosus events
%%%-------------------------------------------------------------------
-module(webgnosus_events).

%% API
-export([
         message/1,
         warning/1,
         alarm/1
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: message/1
%% Returns: ok
%% Description: webgnosus messages written to error logger
%%--------------------------------------------------------------------
%% module start message
message({started, M}) ->
    error_logger:info_msg("starting module: ~p~n", [M]);

%% handle any unspecified messages
message(X) ->
    error_logger:info_msg("~p~n", X).

%%--------------------------------------------------------------------
%% Func: warning/1
%% Returns: ok
%% Description: webgnosus warnings written to error logger
%%--------------------------------------------------------------------
warning(X) ->
    error_logger:warning_msg("~p~n", X).

%%--------------------------------------------------------------------
%% Func: alarm/1
%% Returns: ok
%% Description: webgnosus alarms written to error logger
%%--------------------------------------------------------------------
alarm(X) ->
    error_logger:error_msg("~p~n", X).

%%====================================================================
%%% Internal functions
%%====================================================================
