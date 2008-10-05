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
          find/1,
          count/0,
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
    webgnosus_dbi:create_table(laconica_sites, [{attributes, record_info(fields, laconica_sites)}, {disc_only_copies, [node()]}]).

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

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% generic row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
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
%% Description: delete specified record to database
%%--------------------------------------------------------------------
delete(RootUrl) ->
    webgnosus_dbi:delete_row({laconica_sites, RootUrl}).

%%--------------------------------------------------------------------
%% Func: find/1
%% Description: find models
%%--------------------------------------------------------------------
%% find all models
find(all) ->
    webgnosus_dbi:q(qlc:q([X || X <- mnesia:table(laconica_sites)]));

%% find specified record to database
find(RootUrl) ->
    {atomic, Result} = webgnosus_dbi:read_row({laconica_sites, RootUrl}),
    Result.

%%--------------------------------------------------------------------
%% Func: count/0
%% Description: return row count
%%--------------------------------------------------------------------
count() ->
    webgnosus_dbi:count(laconica_sites).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% model row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: key/1
%% Description: define model key
%%--------------------------------------------------------------------
%% find all models
key(Url) ->
    Url.

%%====================================================================
%%% Internal functions
%%====================================================================
