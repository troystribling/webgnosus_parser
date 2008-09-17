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
%% Description: webgnosus messages written to error logger
%%--------------------------------------------------------------------
%% module start message
message({started, M}) ->
    error_logger:info_msg("starting module: ~p~n", [M]);

%% module stop message
message({stopped, M}) ->
    error_logger:info_msg("stopping module: ~p~n", [M]);

%% retrived public timeline
message({public_timeline, Url}) ->
    error_logger:info_msg("Retrieved public timeline for: ~p~n", [Url]);

%% handle any unspecified messages
message(X) ->
    error_logger:info_msg("~p~n", X).

%%--------------------------------------------------------------------
%% Func: warning/1
%% Description: webgnosus warnings written to error logger
%%--------------------------------------------------------------------
%% mnesia timeout
warning({mnesia_start_timeout, T}) ->
    error_logger:warning_msg("mnesia timeout waiting for tables: ~p~n", T);

%% mnesia start error
warning({mnesia_start_error, M}) ->
    error_logger:warning_msg("mnesia start error: ~p~n", [M]);

%% mnesia start error
warning({session_not_found, M}) ->
    error_logger:warning_msg("session close error not found: ", [M]);

%% handle any unspecified messages
warning(X) ->
    error_logger:warning_msg("~p~n", X).

%%--------------------------------------------------------------------
%% Func: alarm/1
%% Description: webgnosus alarms written to error logger
%%--------------------------------------------------------------------
%% handle any unspecified messages
alarm(X) ->
    error_logger:error_msg("~p~n", X).

%%====================================================================
%%% Internal functions
%%====================================================================
