%%%-------------------------------------------------------------------
%%% laconica utilities
%%%-------------------------------------------------------------------
-module(webgnosus_util).

%% API
-export([
          get_attribute/2,
          get_matches/2
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: get_location_from_headers/1
%% Description: get location from 301 header.
%%--------------------------------------------------------------------
get_attribute(Key, R) ->
    case lists:keysearch(Key, 1, R) of
        {value, {Key, Value}} ->
            Value;
        _ ->
            undefined
    end.

%%--------------------------------------------------------------------
%% Func: get_matches/1
%% Description: return all tags in status
%%--------------------------------------------------------------------
get_matches(S, R) ->
    case regexp:matches(S, R) of
        {match, Matches} ->
            lists:map(
                fun({Pos, Length}) ->
                    lists:sublist(S, Pos, Length)
                end,
                Matches);
        {error, _} ->
            []
    end.
