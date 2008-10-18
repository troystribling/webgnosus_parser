%%%-------------------------------------------------------------------
%%% laconica utilities
%%%-------------------------------------------------------------------
-module(webgnosus_util).

%% API
-export([
          get_attribute/2,
          get_matches/2,
          replace_at_position/3,
          filter/2
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: get_location_from_headers/2
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
%% Func: get_matches/2
%% Description: return all matches to regular expression
%%--------------------------------------------------------------------
get_matches(String, R) ->
    case regexp:matches(String, R) of
        {match, Matches} ->
            lists:map(
                fun({Pos, Length}) ->
                    lists:sublist(String, Pos, Length)
                end,
                Matches);
        {error, _} ->
            []
    end.

%%--------------------------------------------------------------------
%% Func: filter/2
%% Description: return items in list that match regular expression
%%--------------------------------------------------------------------
filter(List, R) ->
    lists:filter(
        fun(L) ->
            case regexp:first_match(L, R) of
                {match, _, _} ->
                    true;
                 _ ->
                    false
            end
         end,
         List).

%%--------------------------------------------------------------------
%% Func: replace_at_position/3
%% Description:replace string at postion.
%%--------------------------------------------------------------------
replace_at_position({Pos, Length}, Rep, Doc) ->    
    {Head, Tail} = lists:split(Pos - 1, Doc),
    NewTail = lists:sublist(Tail, Length + 1, length(Tail) - Length),
    lists:concat([Head, Rep, NewTail]).


