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
          count_by_site/1,
          oldest_by_site/1,
          oldest/0,
          latest/0,
          latest_by_site/1,
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
%% Description: delete specifie record to database
%%--------------------------------------------------------------------
delete({StatusId, UserId}) ->
    Oid = {laconica_statuses, {StatusId, UserId}},
    webgnosus_dbi:delete_row(Oid).

%%--------------------------------------------------------------------
%% Func: find/1
%% Description: find models
%%--------------------------------------------------------------------
%% find all models
find(all) ->
    webgnosus_dbi:q(qlc:q([X || X <- mnesia:table(laconica_statuses)])).

%%--------------------------------------------------------------------
%% Func: count/0
%% Description: return row count
%%--------------------------------------------------------------------
count() ->    
     webgnosus_dbi:map(fun(_X, Sum) -> Sum + 1 end, 0, 
                       qlc:q([X || X <- mnesia:table(laconica_statuses)])).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% model row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: key/1
%% Description: define model key
%%--------------------------------------------------------------------
%% find all models
key({StatusId, UserId, SiteUrl}) ->
    {StatusId, UserId, SiteUrl}.

%%--------------------------------------------------------------------
%% Func: oldest/1
%% Description: return the oldest status.
%%--------------------------------------------------------------------
oldest() ->      
    webgnosus_dbi:map(
                      fun(S, Old) ->  
                          older(S, Old)
                      end, 
                      {}, 
                      qlc:q([S || S <- mnesia:table(laconica_statuses)])).

%%--------------------------------------------------------------------
%% Func: latest/1
%% Description: return latest
%%--------------------------------------------------------------------
latest() ->  
    webgnosus_dbi:map(
                      fun(S, Late) ->  
                          later(S, Late)
                      end, 
                      {}, 
                      qlc:q([S || S <- mnesia:table(laconica_statuses)])).


%%--------------------------------------------------------------------
%% Func: oldest_by_site/1
%% Description: return oldest status for specified site
%%--------------------------------------------------------------------
%% count rows
oldest_by_site(Site) ->      
    webgnosus_dbi:map(
                      fun(S, Old) ->  
                          older(S, Old)
                      end, 
                      {}, 
                      qlc:q([S || S <- mnesia:table(laconica_statuses), S#laconica_statuses.site =:= Site])).

%%--------------------------------------------------------------------
%% Func: latest_by_site/1
%% Description: return latest status for specified site
%%--------------------------------------------------------------------
%% count rows
latest_by_site(Site) ->      
    webgnosus_dbi:map(
                      fun(S, Late) ->  
                          later(S, Late)
                      end, 
                      {}, 
                      qlc:q([S || S <- mnesia:table(laconica_statuses), S#laconica_statuses.site =:= Site])).

%%--------------------------------------------------------------------
%% Func: count_by_site/1
%% Description: return row count for specified site
%%--------------------------------------------------------------------
count_by_site(Site) ->
    webgnosus_dbi:map(fun(_X, Sum) -> Sum + 1 end, 0, 
                      qlc:q([X || X <- mnesia:table(laconica_statuses), X#laconica_statuses.site =:= Site])).

%%====================================================================
%% Internal functions
%%====================================================================        
%%--------------------------------------------------------------------
%% Func: older/2
%% Description: return older status
%%--------------------------------------------------------------------
older(S, Old) ->  
    #laconica_statuses{created_at = OldD} = Old,
    #laconica_statuses{created_at = SD} = S,
    SDSecs = laconica_util:date_to_gregorian_seconds(SD),
    OldSecs = laconica_util:date_to_gregorian_seconds(OldD),
    if 
         SDSecs < OldSecs ->
            S;
        true -> 
            Old
    end.

%%--------------------------------------------------------------------
%% Func: older/2
%% Description: return later status
%%--------------------------------------------------------------------
later(S, Old) ->  
    #laconica_statuses{created_at = OldD} = Old,
    #laconica_statuses{created_at = SD} = S,
    SDSecs = laconica_util:date_to_gregorian_seconds(SD),
    OldSecs = laconica_util:date_to_gregorian_seconds(OldD),
    if 
         SDSecs > OldSecs ->
            S;
        true -> 
            Old
    end.
