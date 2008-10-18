%%%-------------------------------------------------------------------
%%% model interface for webgnosus word database
%%%-------------------------------------------------------------------
-module(webgnosus_word_model).

%% API
-export([
          create_table/0,
          delete_table/0,
          clear_table/0,
          write/1,
          delete/1,
          find/1,
          count/0,
          total_word_count/0,
          word_count/1,
          word/1,
          word_frequency/1,
          write_words/1,
          key/1,
          most_frequent/1,
          count_words/2
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
    webgnosus_dbi:create_table(webgnosus_words, [{attributes, record_info(fields, webgnosus_words)}, {disc_copies, [node()]}]).

%%--------------------------------------------------------------------
%% Func: delete_tables/0
%% Description: delete application database tables
%%--------------------------------------------------------------------
delete_table() ->
    webgnosus_dbi:delete_table(webgnosus_words).

%%--------------------------------------------------------------------
%% Func: clear_tables/0
%% Description: delete all rows in application database tables
%%--------------------------------------------------------------------
clear_table() ->
    webgnosus_dbi:clear_table(webgnosus_words).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% generic row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: write/1
%% Description: write specified record to database
%%--------------------------------------------------------------------
write(R) when is_record(R, webgnosus_words) ->
    webgnosus_dbi:write_row(R);

write(R) when is_list(R) ->
    webgnosus_dbi:write_row({webgnosus_words, 
        webgnosus_util:get_attribute(word, R),
        webgnosus_util:get_attribute(word_count, R),
        webgnosus_util:get_attribute(word_frequency, R)
    });

write(_) ->
    {atomic, error}.

%% return row count
count() ->    
    webgnosus_dbi:count(laconica_statuses).

%%--------------------------------------------------------------------
%% Func: delete/1
%% Description: delete specified record to database
%%--------------------------------------------------------------------
delete(Word) ->
    webgnosus_dbi:delete_row({webgnosus_words, Word}).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% queries
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: find/1
%% Description: find models
%%--------------------------------------------------------------------
%% find all models
find(all) ->
    webgnosus_dbi:q(qlc:q([X || X <- mnesia:table(webgnosus_words)])).

%%--------------------------------------------------------------------
%% Func: most_frequent/1
%% Description: find models
%%--------------------------------------------------------------------
%% return sorted list of most frequent words
most_frequent({count, Count}) ->      
    Result = webgnosus_dbi:fold(
        fun(W, Words) ->  
            larger_count(W, Words, Count)
        end, 
        [], 
        qlc:q([W || W <- mnesia:table(webgnosus_words)])),
    webgnosus_util:values(Result).

%%--------------------------------------------------------------------
%% Func: total_word_count/0
%% Description: find models
%%--------------------------------------------------------------------
%% return sorted list of most frequent words
total_word_count() ->      
    webgnosus_dbi:fold(
        fun(#webgnosus_words{word_count = WordCount}, Count) ->  
            Count + WordCount
        end, 
        0, 
        qlc:q([W || W <- mnesia:table(webgnosus_words)])).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% attributes
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
word(#webgnosus_words{word = Attr}) ->    
    Attr.

word_count(#webgnosus_words{word_count = Attr}) ->    
    Attr.

word_frequency(#webgnosus_words{word_frequency = Attr}) ->    
    Attr.

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% text analysis
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: count_words/2
%% Description: counts words in tokenized document
%%--------------------------------------------------------------------
count_words(Tokens, Words) ->
    lists:foldl(        
        fun(T, W) ->
            case gb_trees:is_defined(T, W) of
                true ->
                    gb_trees:update(T, gb_trees:get(T, W) + 1, W);
                false ->
                    gb_trees:insert(T, 1, W)
            end
        end,
        Words,
        Tokens).
    
%%--------------------------------------------------------------------
%% Func: write_words/1
%% Description: counts words in tokenized document
%%--------------------------------------------------------------------
write_words(Words) ->
    clear_table(),
    Total = lists:sum(gb_trees:values(Words)),
    lists:foreach(        
        fun({W, C}) ->
            write([{word, W}, {word_count, C}, {word_frequency, C/Total}])
        end,
        gb_trees:to_list(Words)).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% model row methods
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: key/1
%% Description: define model key
%%--------------------------------------------------------------------
%% find all models
key(Word) ->
    Word.

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: later/3
%% Description: build list of words with largest count
%%--------------------------------------------------------------------
larger_count(#webgnosus_words{word_count = WordCount} = Word, Large, Count) ->
    if
        length(Large) < Count ->
            lists:keysort(1, [{WordCount, Word} | Large]);
        true ->
            update_larger_count_list(WordCount, Word, Large)
    end.

%%--------------------------------------------------------------------
%% Func: update_late_list/3
%% Description: if status is later add to list.
%%--------------------------------------------------------------------
update_larger_count_list(WordCount, Word, [{LeastLargeCount, _} | _] = Large) ->
    if
        WordCount > LeastLargeCount ->
            [_ | NewLarge] = lists:keysort(1, [{WordCount, Word} | Large]),
            NewLarge;
        true ->
            Large
    end.
