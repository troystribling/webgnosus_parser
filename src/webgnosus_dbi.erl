%%%-------------------------------------------------------------------
%%% webgnosus database utilities
%%%-------------------------------------------------------------------
-module(webgnosus_dbi).

%% API
-export([
          create_table/2,
          delete_table/1,
          clear_table/1,
          write_row/1,
          delete_row/1,
          q/1,
          limit/2,
          map/3
       ]).

%% include
-include_lib("laconica_model.hrl").
-include_lib("stdlib/include/qlc.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% table methods
%%--------------------------------------------------------------------
%% Func: create_table/2
%% Description: create application database tables
%%--------------------------------------------------------------------
create_table(Model, Options) ->
    mnesia:create_table(Model, Options).

%%--------------------------------------------------------------------
%% Func: delete_table/1
%% Description: delete application database tables
%%--------------------------------------------------------------------
delete_table(Model) ->
    mnesia:delete_table(Model).

%%--------------------------------------------------------------------
%% Func: clear_table/1
%% Description: delete all rows in application database tables
%%--------------------------------------------------------------------
clear_table(Model) ->
    mnesia:clear_table(Model).

%%--------------------------------------------------------------------
%% row methods
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Func: write_row/1
%% Description: write given record
%%--------------------------------------------------------------------
write_row(Row) ->
    mnesia:transaction(
        fun() -> 
            mnesia:write(Row)
        end).

%%--------------------------------------------------------------------
%% Func: delete_row/1
%% Description: delete specified by Oid
%%--------------------------------------------------------------------
delete_row(Oid) ->
    mnesia:transaction(
        fun() ->
            mnesia:delete(Oid)
        end).

%%--------------------------------------------------------------------
%% query methods
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Func: q/1
%% Description: evaluate qlc query within transaction
%%--------------------------------------------------------------------
q(Q) ->
    {atomic, Val} = mnesia:transaction(
        fun() ->
             qlc:e(Q)
        end),
    Val.       

%%--------------------------------------------------------------------
%% Func: limit/1
%% Description: limit results of query to count
%%--------------------------------------------------------------------
limit(Q, C) ->
   {atomic, Val} = mnesia:transaction(
        fun() ->
           Cursor = qlc:cursor(Q),
           Result = qlc:next_answers(Cursor, C),
           qlc:delete_cursor(Cursor),
           Result
        end),
    Val.       

%%--------------------------------------------------------------------
%% Func: map/3
%% Description: apply function to query
%%--------------------------------------------------------------------
map(F, I, Q) ->
    {atomic, Val} = mnesia:transaction(
        fun() ->
           qlc:fold(F, I, Q)
        end),
    Val.       

%%====================================================================
%%% Internal functions
%%====================================================================
