%%%-------------------------------------------------------------------
%%% model interface for laconica site
%%%-------------------------------------------------------------------
-module(laconica_site_model).

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
%%--------------------------------------------------------------------
%% table methods
%%--------------------------------------------------------------------
%% Func: create_tables/0
%% Returns: 
%% Description: create application database tables
%%--------------------------------------------------------------------
create_table() ->
    webnosus_dbi:create_table(laconica_site, [{attributes, record_info(fields, laconica_site)}]).

%%--------------------------------------------------------------------
%% Func: delete_tables/0
%% Returns: 
%% Description: delete application database tables
%%--------------------------------------------------------------------
delete_table() ->
    webnosus_dbi:delete_table(laconica_site).

%%--------------------------------------------------------------------
%% Func: clear_tables/0
%% Returns: 
%% Description: delete all rows in application database tables
%%--------------------------------------------------------------------
clear_table() ->
    webnosus_dbi:clear_table(laconica_site).

%%--------------------------------------------------------------------
%% row methods
%%--------------------------------------------------------------------

%%====================================================================
%%% Internal functions
%%====================================================================
