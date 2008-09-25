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
    error_logger:info_msg("retrieved public timeline for: ~p~n", [Url]);

%% performed collection for pid
message({collection, Pid}) ->
    error_logger:info_msg("collecting pid: ~p~n", [Pid]);

%% stoped collection
message({stop_collection, Pid}) ->
    error_logger:info_msg("stopped collecting pid: ~p~n", [Pid]);

%% started collection
message({start_collection, Pid}) ->
    error_logger:info_msg("started collecting pid: ~p~n", [Pid]);

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

%% laconica session not found
warning({session_not_found, M}) ->
    error_logger:warning_msg("session not found: ~p~n", [M]);

%% laconica session is open
warning({session_open, M}) ->
    error_logger:warning_msg("session is open: ~p~n", [M]);

%% http get error
warning({http_get_failed, M}) ->
    error_logger:warning_msg("http get failed for: ~p~n", [M]);

%% XML parse error
warning({xml_parse_failed, [Url, X]}) ->
    error_logger:warning_msg("xml parse failed for: ~p~ncaught exception: ~p~n", [Url, X]);

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
