%%%-------------------------------------------------------------------
%%% model interface for laconica status
%%%-------------------------------------------------------------------
-module(laconica_status_model).

%% API
-export([
          create_table/0,
          delete_table/0,
          clear_table/0,
          write/1,
          delete/1,
          find/1,
          count/0,
          count/1,
          get_tags/1,
          get_urls/1,
          tokenize_and_resolve_urls/1,
          tokenize/1,
          count_words/2,
          count_words/1,
          count_words/0,
          oldest/0,
          oldest/1,
          latest/0,
          latest/1,
          key/1
       ]).

%% include
-include_lib("laconica_model.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%====================================================================
%% API
%%====================================================================
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% generic table methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% Func: create_tables/0
%% Description: create application database tables
%%--------------------------------------------------------------------
create_table() ->
    webgnosus_dbi:create_table(laconica_statuses, 
        [{attributes, record_info(fields, laconica_statuses)}, {disc_only_copies, [node()]}]).

%%--------------------------------------------------------------------
%% Func: delete_tables/0
%% Description: delete application database tables
%%--------------------------------------------------------------------
delete_table() ->
    webgnosus_dbi:delete_table(laconica_statuses).

%%--------------------------------------------------------------------
%% Func: clear_tables/0
%% Description: delete all rows in application database tables
%%--------------------------------------------------------------------
clear_table() ->
    webgnosus_dbi:clear_table(laconica_statuses).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% generic row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: write/1
%% Description: write specified record to database
%%--------------------------------------------------------------------
write(R) when is_record(R, laconica_statuses) ->
    webgnosus_dbi:write_row(R);

write(_) ->
    {atomic, error}.

%%--------------------------------------------------------------------
%% Func: delete/1
%% Description: delete specified record to database
%%--------------------------------------------------------------------
delete({StatusId, UserId, Site}) ->
    webgnosus_dbi:delete_row({laconica_statuses, {StatusId, UserId, Site}}).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% queries
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: find/1
%% Description: find models
%%--------------------------------------------------------------------
%% find all models
find(all) ->
    webgnosus_dbi:q(qlc:q([S || S <- mnesia:table(laconica_statuses)]));

%% find all models where text matches specified rexp
find({text, R}) ->
    webgnosus_dbi:q(qlc:q([S || S <- mnesia:table(laconica_statuses), text_contains(S, R)]));

%% find all models where text matches specified rexp and specified site
find({{site, Site}, {text, R}}) ->
    webgnosus_dbi:q(qlc:q([S || S <- mnesia:table(laconica_statuses), text_contains(S, R), S#laconica_statuses.site =:= Site]));

%% find specified record to database
find({StatusId, UserId, Site}) ->
    case webgnosus_dbi:read_row({laconica_statuses, {StatusId, UserId, Site}}) of
        {atomic, []} ->
            error;
        {atomic, Result} ->
            hd(Result)
     end.

%%--------------------------------------------------------------------
%% Func: latest/1
%% Description: return latest
%%--------------------------------------------------------------------
%% return latest status
latest() ->  
    webgnosus_dbi:fold(
        fun(S, Late) ->  
            later(S, Late)
        end,
        {}, 
        qlc:q([S || S <- mnesia:table(laconica_statuses)])).

%% return latest status for specified site
latest({site, Site}) ->      
    webgnosus_dbi:fold(
        fun(S, Late) ->  
            later(S, Late)
        end, 
        {}, 
        qlc:q([S || S <- mnesia:table(laconica_statuses), S#laconica_statuses.site =:= Site]));

%% return latest count of statuses
latest({count, Count}) ->      
    Result = webgnosus_dbi:fold(
        fun(S, Late) ->  
            later(S, Late, Count)
        end, 
        [], 
        qlc:q([S || S <- mnesia:table(laconica_statuses)])),
    webgnosus_util:values(Result);

%% return latest count of statuses for specified site
latest({{site, Site}, {count, Count}}) ->      
    Result = webgnosus_dbi:fold(
        fun(S, Late) ->  
            later(S, Late, Count)
        end, 
        [], 
        qlc:q([S || S <- mnesia:table(laconica_statuses), S#laconica_statuses.site =:= Site])),
    webgnosus_util:values(Result).
    
%%--------------------------------------------------------------------
%% Func: count/0
%% Description: return row count
%%--------------------------------------------------------------------
count() ->    
    webgnosus_dbi:count(laconica_statuses).

%%--------------------------------------------------------------------
%% Func: count/1
%% Description: return row count for specified site
%%--------------------------------------------------------------------
%% return row count for specified site
count({site, Site}) ->
    webgnosus_dbi:fold(
        fun(_S, Sum) -> 
            Sum + 1
        end, 
        0, 
        qlc:q([S || S <- mnesia:table(laconica_statuses), S#laconica_statuses.site =:= Site])).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% model row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: key/1
%% Description: define model key
%%--------------------------------------------------------------------
%% model key
key({StatusId, UserId, SiteUrl}) ->
    {StatusId, UserId, SiteUrl}.

%%--------------------------------------------------------------------
%% Func: oldest/1
%% Description: return the oldest status.
%%--------------------------------------------------------------------
%% return oldest status
oldest() ->      
    webgnosus_dbi:fold(
        fun(S, Old) ->  
            older(S, Old)
        end, 
        {}, 
        qlc:q([S || S <- mnesia:table(laconica_statuses)])).

%% return oldest status for specified site
oldest({site, Site}) ->      
    webgnosus_dbi:fold(
        fun(S, Old) ->  
            older(S, Old)
        end, 
        {}, 
        qlc:q([S || S <- mnesia:table(laconica_statuses), S#laconica_statuses.site =:= Site])).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% text analysis
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: tokenize_and_resolve_urls/1
%% Description: tokenize status text and resolve redirected URLs
%%--------------------------------------------------------------------
tokenize_and_resolve_urls(#laconica_statuses{text = S}) ->
   Toks =  webgnosus_text:tokenize(S),
   Urls = get_urls(Toks),
   MapUrls = lists:map(
        fun(U) ->  
            webgnosus_http:get_redirect_url(U)
        end, 
        Urls),
    lists:append(
        lists:subtract(Toks, Urls),
        MapUrls
    ).
   
%%--------------------------------------------------------------------
%% Func: tokenize_amd_resolve_urls/1
%% Description: tokenize status text and resolve redirected URLs
%%--------------------------------------------------------------------
tokenize(#laconica_statuses{text = S}) ->
   webgnosus_text:tokenize(S).
    
%%--------------------------------------------------------------------
%% Func: get_tags/1
%% Description: return all tags in status
%%--------------------------------------------------------------------
get_tags(Tokens) when is_list(Tokens) ->
    webgnosus_util:filter(Tokens, "^#").

%%--------------------------------------------------------------------
%% Func: get_urls/1
%% Description: return all urls in status
%%--------------------------------------------------------------------
get_urls(Tokens) when is_list(Tokens) ->
    webgnosus_util:filter(Tokens, "^http:").

%%--------------------------------------------------------------------
%% Func: count_words
%% Description: determine word counts for status text
%%--------------------------------------------------------------------
% count words for all status messages and save
count_words() ->
    webgnosus_word_model:write_words(
        count_words(qlc:q([S || S <- mnesia:table(laconica_statuses)]))).

% count words for specified list of status messages
count_words(StatusList) when is_list(StatusList)->
    webgnosus_dbi:fold(
        fun(S, Words) ->  
            count_words(S, Words)
        end, 
        gb_trees:empty(), 
        StatusList).

% count words for specified status message
count_words(Status, Words) ->
    webgnosus_word_model:count_words(tokenize_and_resolve_urls(Status), Words).

%%====================================================================
%% Internal functions
%%====================================================================        
%%--------------------------------------------------------------------
%% Func: older/2
%% Description: return older status
%%--------------------------------------------------------------------
older(S, {}) ->  
    S;

older(S, Old) ->  
    SSecs  = date_to_gregorian_seconds(S),
    OldSecs = date_to_gregorian_seconds(Old),
    if 
         SSecs < OldSecs ->
            S;
        true -> 
            Old
    end.

%%--------------------------------------------------------------------
%% Func: later/2
%% Description: return later status
%%--------------------------------------------------------------------
later(S, {}) ->  
    S;

later(S, Late) ->  
    SSecs   = date_to_gregorian_seconds(S),
    LateSecs = date_to_gregorian_seconds(Late),
    if 
         SSecs > LateSecs ->
            S;
        true -> 
            Late
    end.
    
%%--------------------------------------------------------------------
%% Func: later/3
%% Description: add status to late list if later than any in list
%%--------------------------------------------------------------------
later(S, Late, Count) ->
    SSecs   = date_to_gregorian_seconds(S),  
    LateSize = length(Late),
    if
        LateSize < Count ->
            lists:keysort(1, [{SSecs, S} | Late]);
        true ->
            update_later_list(SSecs, S, Late)
    end.

%%--------------------------------------------------------------------
%% Func: date_to_gregorian_seconds/1
%% Description: convert laconica date format gregorian seconds.
%%--------------------------------------------------------------------
date_to_gregorian_seconds(#laconica_statuses{created_at = SD}) ->
    laconica_util:date_to_gregorian_seconds(SD).

%%--------------------------------------------------------------------
%% Func: tesxt_contains/2
%% Description: true if specified rexp matches status text.
%%--------------------------------------------------------------------
text_contains(#laconica_statuses{text = T}, R) ->
    case regexp:first_match(T, R) of
        {match, _, _} ->
            true;
        _ -> 
            false
    end.
    
%%--------------------------------------------------------------------
%% Func: update_late_list/3
%% Description: if status is later add to list.
%%--------------------------------------------------------------------
update_later_list(StatusSecs, Status, [{LeastLate, _} | _] = Late) ->
    if
        StatusSecs > LeastLate ->
            [_ | NewLate] = lists:keysort(1, [{StatusSecs, Status} | Late]),
            NewLate;
        true ->
            Late
    end.
