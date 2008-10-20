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
          read_row/1,
          q/1,
          limit/2,
          fold/3,
          foreach/2,
          transaction/1,
          count/1
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
%% Func: transaction/1
%% Description: evaluate qlc query within transaction
%%--------------------------------------------------------------------
transaction(F) ->
     case mnesia:transaction(F) of
         {atomic, Val} ->
             Val;
         _ ->
             aborted
     end.
               

%%--------------------------------------------------------------------
%% row methods
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Func: write_row/1
%% Description: write given record
%%--------------------------------------------------------------------
write_row(Row) ->
    transaction(
        fun() -> 
            mnesia:write(Row)
        end).

%%--------------------------------------------------------------------
%% Func: read_row/1
%% Description: write given record
%%--------------------------------------------------------------------
read_row(Row) ->
    transaction(
        fun() -> 
            mnesia:read(Row)
        end).

%%--------------------------------------------------------------------
%% Func: delete_row/1
%% Description: delete specified by Oid
%%--------------------------------------------------------------------
delete_row(Oid) ->
    transaction(
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
    transaction(
        fun() ->
             qlc:e(Q)
        end).       

%%--------------------------------------------------------------------
%% Func: limit/1
%% Description: limit results of query to count
%%--------------------------------------------------------------------
limit(Q, C) ->
   transaction(
        fun() ->
           Cursor = qlc:cursor(Q),
           Result = qlc:next_answers(Cursor, C),
           qlc:delete_cursor(Cursor),
           Result
        end).       

%%--------------------------------------------------------------------
%% Func: fold/3
%% Description: apply function to query
%%--------------------------------------------------------------------
fold(F, I, Q) ->
    transaction(
        fun() ->
           qlc:fold(F, I, Q)
        end).       

%%--------------------------------------------------------------------
%% Func: foreach/1
%% Description: apply function to each record in table
%%--------------------------------------------------------------------
foreach(F, Table) ->
    Result = transaction(
        fun() -> 
            mnesia:first(Table) 
        end),
    case Result of
        aborted ->
            aborted;
        FirstKey ->
            foreach(F, Table, FirstKey)
    end.
     
foreach(_, _, '$end_of_table') ->
    ok;       

foreach(F, Table, ThisKey) ->
    case read_row({Table, ThisKey}) of
        aborted ->
            aborted;
        Row ->
            F(Row)
    end,
    Result = transaction(
        fun() ->
            mnesia:next(Table, ThisKey)
        end) ,
    case Result of
        aborted ->
            aborted;
        NextKey ->
            foreach(F, Table, NextKey)
    end.

%%--------------------------------------------------------------------
%% Func: count/0
%% Description: return row count
%%--------------------------------------------------------------------
%% return row count
count(Table) ->    
     webgnosus_dbi:fold(
         fun(_S, Sum) -> 
             Sum + 1 
         end, 
         0,
         qlc:q([S || S <- mnesia:table(Table)])).

%%====================================================================
%%% Internal functions
%%====================================================================
