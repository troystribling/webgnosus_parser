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
message({started, M, Args}) ->
    error_logger:info_msg("starting module: ~p~n~p~n", [M, Args]);

%% module stop message
message({stopped, M}) ->
    error_logger:info_msg("stopping module: ~p~n", [M]);

%% handle any unspecified messages
message(X) ->
    error_logger:info_msg("~p~n", X).

%%--------------------------------------------------------------------
%% Func: warning/1
%% Returns: ok
%% Description: webgnosus warnings written to error logger
%%--------------------------------------------------------------------
%% mnesia timeout
warning({mnesia_start_timeout, T}) ->
    error_logger:warning_msg("mnesia timeout waiting for tables: ~p~n", T);

%% mnesia start error
warning({mnesia_start_error, R}) ->
    error_logger:warning_msg("mnesia start error: ~p~n", [R]);

%% handle any unspecified messages
warning(X) ->
    error_logger:warning_msg("~p~n", X).

%%--------------------------------------------------------------------
%% Func: alarm/1
%% Returns: ok
%% Description: webgnosus alarms written to error logger
%%--------------------------------------------------------------------
%% handle any unspecified messages
alarm(X) ->
    error_logger:error_msg("~p~n", X).

%%====================================================================
%%% Internal functions
%%====================================================================
