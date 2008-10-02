%%%-------------------------------------------------------------------
%%% laconica utilities
%%%-------------------------------------------------------------------
-module(laconica_util).

%% API
-export([
          date_to_rfc1123/1,
          date_to_gregorian_seconds/1
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: date_to_rfc1123/1
%% Description: convert laconica date format to rfc 1123.
%%--------------------------------------------------------------------
date_to_rfc1123(Date) ->
    case regexp:split(Date, " +") of
        {ok, D} -> D;
         X  -> X
    end.

%%====================================================================
%%% Internal functions
%%====================================================================
