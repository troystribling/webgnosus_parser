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
        {ok, D} ->             
            httpd_util:convert_request_date(lists:nth(1,D) ++ ", " ++ lists:nth(3,D) ++ " " ++ lists:nth(2,D) ++ " " ++
                                             lists:nth(6,D) ++ " " ++ prepend_zero_to_time(lists:nth(4,D)));
         X  -> X
    end.

%%--------------------------------------------------------------------
%% Func: date_to_rfc1123/1
%% Description: convert laconica date format gregorian seconds.
%%--------------------------------------------------------------------
date_to_gregorian_seconds(Date) ->
    calendar:datetime_to_gregorian_seconds(date_to_rfc1123(Date)).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: date_to_rfc1123/1
%% Description: convert laconica date format to rfc 1123.
%%--------------------------------------------------------------------
prepend_zero_to_time(Time) ->
    case regexp:first_match(Time, "^0") of
        {match, _, _} ->
            Time;
        _ -> 
            "0" ++ Time
    end.
