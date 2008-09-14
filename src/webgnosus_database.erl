%%%-------------------------------------------------------------------
%%% webgnosus database utilities
%%%-------------------------------------------------------------------
-module(webgnosus_dbi).

%% API
-export([
          create_table/0,
          delete_table/0,
          clear_table/0,
          write/1,
          delete/1
       ]).

%%====================================================================
%% API
%%====================================================================
%% Func: create_tables/0
%% Returns: 
%% Description: create application database tables
%%--------------------------------------------------------------------
create_table(Model, Options) ->
    mnesia:create_table(Model, Options).

%%--------------------------------------------------------------------
%% Func: delete_tables/0
%% Returns: 
%% Description: delete application database tables
%%--------------------------------------------------------------------
delete_table(Model) ->
    mnesia:delete_table(Model).

%%--------------------------------------------------------------------
%% Func: clear_tables/0
%% Returns: 
%% Description: delete all rows in application database tables
%%--------------------------------------------------------------------
clear_table(Model) ->
    mnesia:clear_table(Model).

%%====================================================================
%%% Internal functions
%%====================================================================
