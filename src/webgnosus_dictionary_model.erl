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
          language/1,
          in_dictionary/1,
          load_dictionary/1
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
word(#webgnosus_dictionary{word = Attr}) ->    
    Attr.

language(#webgnosus_dictionary{language = Attr}) ->    
    Attr.

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% text analysis
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: in_dictionary/1
%% Description: determine number of matches between tokens and 
%%              specified language
%%--------------------------------------------------------------------
in_dictionary([{document, Document}, {dictionary, Dictionary}]) ->
    case regexp:first_match(Document, Dictionary) of
        {match, _, _} ->
            true;
        _ ->
            false
    end;

in_dictionary([{count, Count}, {document, Document}, {dictionary, Dictionary}]) ->
    case regexp:matches(Document, Dictionary) of
        {match, []} ->
            false;
        {match, Matches} ->
            NumberMatches = length(Matches),
            if
                NumberMatches < Count ->
                    false;
                true ->
                    true                
            end;
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% Func: load_dictionary/1
%% Description: load text dump of table
%%--------------------------------------------------------------------
load_dictionary(Language) ->
    Words = lists:map(
        fun (W) ->
            word(W)
        end,
        find(Language)),
    build_dictionary("", Words).
    
build_dictionary("", [Word|Words]) ->    
    build_dictionary(lists:concat([build_dictionary_entry(Word)]), Words);

build_dictionary(Dictionary, []) ->    
    Dictionary;

build_dictionary(Dictionary, [Word|Words]) ->    
    build_dictionary(lists:concat([Dictionary, "|", build_dictionary_entry(Word)]), Words).

build_dictionary_entry(Word) ->    
    lists:concat([lists:concat(["^", Word, "\\s|"]), lists:concat(["\\s", Word, "\\s|"]), 
        lists:concat(["\\s", Word, "$|"]), lists:concat(["\\s", Word, "\\.|"]), lists:concat(["\\s", Word, "\\,"])]).
    
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
