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
          delete/1,
          find/1
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
%% Func: create_tables/0
%% Description: create application database tables
%%--------------------------------------------------------------------
create_table() ->
    webgnosus_dbi:create_table(laconica_sites, [{attributes, record_info(fields, laconica_sites)}, {disc_copies, [node()]}]).

%%--------------------------------------------------------------------
%% Func: delete_tables/0
%% Description: delete application database tables
%%--------------------------------------------------------------------
delete_table() ->
    webgnosus_dbi:delete_table(laconica_sites).

%%--------------------------------------------------------------------
%% Func: clear_tables/0
%% Description: delete all rows in application database tables
%%--------------------------------------------------------------------
clear_table() ->
    webgnosus_dbi:clear_table(laconica_sites).

%%--------------------------------------------------------------------
%% row methods
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% Func: write/1
%% Description: write specified record to database
%%--------------------------------------------------------------------
write(R) when is_record(R, laconica_sites) ->
    webgnosus_dbi:write_row(R);
write(_) ->
    {atomic, error}.

%%--------------------------------------------------------------------
%% Func: delete/1
%% Description: delete specifie record to database
%%--------------------------------------------------------------------
delete(Root_url) ->
    Oid = {laconica_sites, Root_url},
    webgnosus_dbi:delete_row(Oid).

%%--------------------------------------------------------------------
%% Func: find/1
%% Description: find models
%%--------------------------------------------------------------------
%% find all models
find(all) ->
    webgnosus_dbi:q(qlc:q([X || X <- mnesia:table(laconica_sites)])).

%%====================================================================
%%% Internal functions
%%====================================================================
