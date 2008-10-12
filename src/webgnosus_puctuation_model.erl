%%%-------------------------------------------------------------------
%%% model interface for webgnosus word database
%%%-------------------------------------------------------------------
-module(webgnosus_punctuation_model).

%% API
-export([
          create_table/0,
          delete_table/0,
          clear_table/0,
          write/1,
          delete/1,
          find/1,
          load/0,
          count/0,
          key/1,
          regexp/1,
          word/1,
          type/1
       ]).

%% include
-include_lib("webgnosus_model.hrl").
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
    webgnosus_dbi:create_table(webgnosus_puctuation, 
        [{attributes, record_info(fields, webgnosus_puctuation)}, {disc_copies, [node()]}, {type, bag}]).

%%--------------------------------------------------------------------
%% Func: delete_tables/0
%% Description: delete application database tables
%%--------------------------------------------------------------------
delete_table() ->
    webgnosus_dbi:delete_table(webgnosus_puctuation).

%%--------------------------------------------------------------------
%% Func: clear_tables/0
%% Description: delete all rows in application database tables
%%--------------------------------------------------------------------
clear_table() ->
    webgnosus_dbi:clear_table(webgnosus_puctuation).

%%--------------------------------------------------------------------
%% Func: load/0
%% Description: load text dump of table
%%--------------------------------------------------------------------
load() ->
    mnesia:load_textfile("webgnosus_puctuation.dat").

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% generic row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: write/1
%% Description: write specified record to database
%%--------------------------------------------------------------------
write(R) when is_record(R, webgnosus_puctuation) ->
    webgnosus_dbi:write_row(R);

write({punctuation, Word}) ->
    webgnosus_dbi:write_row({webgnosus_puctuation, Word, punctuation});

write({smiley, Word}) ->
    webgnosus_dbi:write_row({webgnosus_puctuation, Word, smiley});

write(_) ->
    {atomic, error}.

%%--------------------------------------------------------------------
%% Func: delete/1
%% Description: delete specified record to database
%%--------------------------------------------------------------------
delete(Word) ->
    webgnosus_dbi:delete_row({webgnosus_puctuation, Word}).

%%--------------------------------------------------------------------
%% Func: find/1
%% Description: find models
%%--------------------------------------------------------------------
%% find all models
find(all) ->
    webgnosus_dbi:q(qlc:q([W || W <- mnesia:table(webgnosus_puctuation)]));

%% find all punctuation
find(punctuation) ->
    webgnosus_dbi:q(qlc:q([W || W <- mnesia:table(webgnosus_puctuation), W#webgnosus_puctuation.type =:= punctuation]));

%% find all smileys
find(smiley) ->
    webgnosus_dbi:q(qlc:q([W || W <- mnesia:table(webgnosus_puctuation), W#webgnosus_puctuation.type =:= smiley])).

%%--------------------------------------------------------------------
%% Func: count/0
%% Description: return row count
%%--------------------------------------------------------------------
count() ->    
    webgnosus_dbi:count(webgnosus_puctuation).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% attributes
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
regexp(#webgnosus_puctuation{regexp = RegExp}) ->    
    RegExp.

word(#webgnosus_puctuation{word = Word}) ->    
    Word.

type(#webgnosus_puctuation{type = Type}) ->    
    Type.

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% model row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: key/1
%% Description: define model key
%%--------------------------------------------------------------------
key(Word) ->
    Word.

%%====================================================================
%%% Internal functions
%%====================================================================
