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
          last_by_site/2,
          list_last_by_site/2,
          first/1,
          list_first/1,
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
    webgnosus_dbi:create_table(laconica_statuses, [{attributes, record_info(fields, laconica_statuses)}, {disc_only_copies, [node()]}]).

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
    {atomic, Val} = mnesia:transaction(
        fun() ->
           qlc:fold(fun(_X, Sum) -> Sum + 1 end, 0, qlc:q([X || X <- mnesia:table(laconica_statuses)]))
        end),
    Val.       

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
%% Func: first/1
%% Description: sort models by key and return specifed number
%%              with smallest value.
%%--------------------------------------------------------------------
%% count rows
first(Count) ->  
    
    SortedQH = qlc:sort(qlc:q([X || X <- mnesia:table(laconica_statuses)]),
                {order, 
                    fun(Status1, Status2) ->
                      order_by_status_id_ascending(Status1, Status2)
                    end}),

   % and run the query
   {atomic, Val} = mnesia:transaction(
        fun() ->
           Cursor = qlc:cursor(SortedQH),
           Result = qlc:next_answers(Cursor, Count),
           qlc:delete_cursor(Cursor),
           Result
        end),
    Val.       

%%--------------------------------------------------------------------
%% Func: last_by_status_id/2
%% Description: sort models by key and return specifed number
%%              with largest value for specified site.
%%--------------------------------------------------------------------
%% count rows
last_by_site(Site, LastCount) ->  
    
    SortedQH = qlc:sort(qlc:q([X || X <- mnesia:table(laconica_statuses), X#laconica_statuses.site =:= Site]),
                {order, 
                    fun(Status1, Status2) ->
                      order_by_status_id_descending(Status1, Status2)
                    end}),

   % and run the query
   {atomic, Val} = mnesia:transaction(
        fun() ->
           Cursor = qlc:cursor(SortedQH),
           Result = qlc:next_answers(Cursor, LastCount),
           qlc:delete_cursor(Cursor),
           Result
        end),
    Val.       

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% format output
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: list_last_by_status_id/2
%% Description: sort models by key and return specifed number
%%              with largest value.
%%--------------------------------------------------------------------
list_last_by_site(Site, LastCount) ->  
    
    Display = fun(Status) -> 
        io:format("~p~n~p~n~p~n~n", [Status#laconica_statuses.screen_name, Status#laconica_statuses.created_at, Status#laconica_statuses.text])
    end,
    
    [Display(S) || S <- last_by_site(Site, LastCount)],
    ok.

%%--------------------------------------------------------------------
%% Func: list_first/1
%% Description: sort models by key and return specifed number
%%              with smallest value.
%%--------------------------------------------------------------------
list_first(Count) ->  
    
    Display = fun(Status) -> 
        io:format("~p~n~p~n~p~n~n", [Status#laconica_statuses.screen_name, Status#laconica_statuses.created_at, Status#laconica_statuses.text])
    end,
    
    [Display(S) || S <- first(Count)],
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: order_by_status_id_descending/2
%% Description: sort by descending status_id.
%%--------------------------------------------------------------------
order_by_status_id_descending(Status1, Status2) ->
    {StatusId1, _, _} = Status1#laconica_statuses.status_id,
    {StatusId2, _, _} = Status2#laconica_statuses.status_id,
    StatusId1 > StatusId2.
        
%%--------------------------------------------------------------------
%% Func: order_by_status_id_ascending/2
%% Description: sort by descending status_id.
%%--------------------------------------------------------------------
order_by_status_id_ascending(Status1, Status2) ->
    {StatusId1, _, _} = Status1#laconica_statuses.status_id,
    {StatusId2, _, _} = Status2#laconica_statuses.status_id,
    StatusId1 < StatusId2.
        
