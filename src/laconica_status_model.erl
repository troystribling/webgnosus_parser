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
          tokenize_and_resolve_urls/1,
          tokenize/1,
          count_words/0,
          count_words/2,
          prepare/1,
          language/1,
          not_language/1,
          oldest/0,
          oldest/1,
          latest/0,
          latest/1,
          text/1,
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
    error.

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
find([{site, Site}, {text, R}]) ->
    webgnosus_dbi:q(qlc:q([S || S <- mnesia:table(laconica_statuses), text_contains(S, R), S#laconica_statuses.site =:= Site]));

%% find specified record to database
find({StatusId, UserId, Site}) ->
    case webgnosus_dbi:read_row({laconica_statuses, {StatusId, UserId, Site}}) of
        [] ->
            error;
        Result ->
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

%% return latest count of statuses
latest([{language, Language}, {count, Count}]) ->      
    Dictionary = webgnosus_dictionary_model:load_dictionary(Language),
    Result = webgnosus_dbi:fold(
        fun(S, Late) ->  
            later(S, Late, Count)
        end, 
        [], 
        qlc:q([S || S <- mnesia:table(laconica_statuses), in_dictionary(S, Dictionary)])),
    webgnosus_util:values(Result);

%% return latest count of statuses for specified site
latest([{site, Site}, {count, Count}]) ->      
    Result = webgnosus_dbi:fold(
        fun(S, Late) ->  
            later(S, Late, Count)
        end, 
        [], 
        qlc:q([S || S <- mnesia:table(laconica_statuses), S#laconica_statuses.site =:= Site])),
    webgnosus_util:values(Result).
    
%%--------------------------------------------------------------------
%% Func: language/1
%% Description: status messages in specified language
%%--------------------------------------------------------------------
language(Language) ->      
    Dictionary = webgnosus_dictionary_model:load_dictionary(Language),
    webgnosus_dbi:q(qlc:q([S || S <- mnesia:table(laconica_statuses), in_dictionary(S, Dictionary)])).

not_language(Language) ->      
    Dictionary = webgnosus_dictionary_model:load_dictionary(Language),
    webgnosus_dbi:q(qlc:q([S || S <- mnesia:table(laconica_statuses), not in_dictionary(S, Dictionary)])).
    
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
%% attributes
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
text(#laconica_statuses{text = Attr}) ->    
    Attr.

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% text analysis
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: tokenize_and_resolve_urls/1
%% Description: tokenize status text and resolve redirected URLs
%%--------------------------------------------------------------------
tokenize_and_resolve_urls(#laconica_statuses{text = S}) ->
   laconica_text:tokenize_and_resolve_urls(S).

%%--------------------------------------------------------------------
%% Func: tokenize/1
%% Description: tokenize status text and resolve redirected URLs
%%--------------------------------------------------------------------
tokenize(#laconica_statuses{text = S}) ->
  laconica_text:tokenize(webgnosus_text:to_lower(S)).

%%--------------------------------------------------------------------
%% Func: prepare/1
%% Description: tokenize status text and resolve redirected URLs
%%--------------------------------------------------------------------
prepare(#laconica_statuses{text = S}) ->
  laconica_text:prepare(webgnosus_text:to_lower(S)).

%%--------------------------------------------------------------------
%% Func: in_dictionary/1
%% Description: return true if status is in specified language
%%--------------------------------------------------------------------
in_dictionary(#laconica_statuses{text = S}, Dictionary) ->
   laconica_text:in_dictionary(webgnosus_text:to_lower(S), Dictionary).

%%--------------------------------------------------------------------
%% Func: count_words
%% Description: determine word counts for english status text
%%--------------------------------------------------------------------
% count words for all status messages and save
count_words() ->
    webgnosus_word_model:clear_table(),
    Dictionary = webgnosus_dictionary_model:load_dictionary(english),
    webgnosus_dbi:foreach(
        fun(S) ->  
            count_words(hd(S), Dictionary)
        end, 
        laconica_statuses).

% count words for specified status message and language Dictionary
count_words(#laconica_statuses{text = S}, Dictionary) ->
    LcS = webgnosus_text:to_lower(S),
    case laconica_text:in_dictionary(LcS, Dictionary) of 
        true ->
            webgnosus_word_model:count_words(
                tokenize(
                    webgnosus_text:replace_shortened_urls(LcS)));
        false ->
            void
    end.

%%====================================================================
%% Internal functions
%%====================================================================        
%%--------------------------------------------------------------------
%% Func: older/2
%% Description: return older status
%%--------------------------------------------------------------------
older(S, {}) ->  
    S;

older(Status, Old) ->  
    StatusSecs  = date_to_gregorian_seconds(Status),
    OldSecs = date_to_gregorian_seconds(Old),
    if 
         StatusSecs < OldSecs ->
            Status;
        true -> 
            Old
    end.

%%--------------------------------------------------------------------
%% Func: later/2
%% Description: return later status
%%--------------------------------------------------------------------
later(S, {}) ->  
    S;

later(Status, Late) ->  
    StatusSecs   = date_to_gregorian_seconds(Status),
    LateSecs = date_to_gregorian_seconds(Late),
    if 
         StatusSecs > LateSecs ->
            Status;
        true -> 
            Late
    end.
    
%%--------------------------------------------------------------------
%% Func: later/3
%% Description: add status to late list if later than any in list
%%--------------------------------------------------------------------
later(Status, Late, Count) ->
    StatusSecs   = date_to_gregorian_seconds(Status),  
    LateSize = length(Late),
    if
        LateSize < Count ->
            lists:keysort(1, [{StatusSecs, Status} | Late]);
        true ->
            update_later_list(StatusSecs, Status, Late)
    end.

%%--------------------------------------------------------------------
%% Func: date_to_gregorian_seconds/1
%% Description: convert laconica date format gregorian seconds.
%%--------------------------------------------------------------------
date_to_gregorian_seconds(#laconica_statuses{created_at = SD}) ->
    laconica_util:date_to_gregorian_seconds(SD).

%%--------------------------------------------------------------------
%% Func: text_contains/2
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
