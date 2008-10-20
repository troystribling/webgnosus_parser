%%%-------------------------------------------------------------------
%%% model interface for webgnosus dictionary database
%%%-------------------------------------------------------------------
-module(webgnosus_dictionary_model).

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
          word/1,
          match_language/2,
          language/1
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
    webgnosus_dbi:create_table(webgnosus_dictionary, 
        [{attributes, record_info(fields, webgnosus_dictionary)}, {disc_copies, [node()]}]).

%%--------------------------------------------------------------------
%% Func: delete_tables/0
%% Description: delete application database tables
%%--------------------------------------------------------------------
delete_table() ->
    webgnosus_dbi:delete_table(webgnosus_dictionary).

%%--------------------------------------------------------------------
%% Func: clear_tables/0
%% Description: delete all rows in application database tables
%%--------------------------------------------------------------------
clear_table() ->
    webgnosus_dbi:clear_table(webgnosus_dictionary).

%%--------------------------------------------------------------------
%% Func: load/0
%% Description: load text dump of table
%%--------------------------------------------------------------------
load() ->
    mnesia:load_textfile("webgnosus_dictionary.dat").

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% generic row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: write/1
%% Description: write specified record to database
%%--------------------------------------------------------------------
write(R) when is_record(R, webgnosus_dictionary) ->
    webgnosus_dbi:write_row(R);

write({english, Word}) ->
    webgnosus_dbi:write_row({webgnosus_dictionary, Word, english});

write(_) ->
    error.

%%--------------------------------------------------------------------
%% Func: delete/1
%% Description: delete specified record to database
%%--------------------------------------------------------------------
delete(Word) ->
    webgnosus_dbi:delete_row({webgnosus_dictionary, Word}).

%%--------------------------------------------------------------------
%% Func: find/1
%% Description: find models
%%--------------------------------------------------------------------
%% find all models
find(all) ->
    webgnosus_dbi:q(qlc:q([W || W <- mnesia:table(webgnosus_dictionary)]));

%% find all english words
find(english) ->
    webgnosus_dbi:q(qlc:q([W || W <- mnesia:table(webgnosus_dictionary), W#webgnosus_dictionary.language =:= english])).

%%--------------------------------------------------------------------
%% Func: count/0
%% Description: return row count
%%--------------------------------------------------------------------
count() ->    
    webgnosus_dbi:count(webgnosus_dictionary).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% attributes
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
word(#webgnosus_dictionary{word = Word}) ->    
    Word.

language(#webgnosus_dictionary{language = Language}) ->    
    Language.

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% text analysis
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: is_english/1
%% Description: determine number of matches between tokens and 
%%              specified language
%%--------------------------------------------------------------------
match_language(Tokens, Language) ->
    webgnosus_dbi:fold(
        fun(#webgnosus_dictionary{word = Word}, Count) ->  
            case lists:member(Word, Tokens) of
                true ->
                    Count + 1;
                false ->
                    Count
            end
         end, 
        0, 
        qlc:q([W || W <- mnesia:table(laconica_statuses), W#webgnosus_dictionary.language =:= Language])).
    

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
