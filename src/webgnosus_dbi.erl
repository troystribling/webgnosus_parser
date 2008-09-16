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
          q/1
       ]).

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

%%====================================================================
%%% Internal functions
%%====================================================================
