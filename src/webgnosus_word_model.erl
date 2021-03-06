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
          count/1,
          word/1,
          frequency/1,
          pos/1,
          key/1,
          most_frequent/1,
          dump/2,
          total_word_count/0,
          calculate_word_frequency/0,
          count_words/1
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
    webgnosus_dbi:create_table(webgnosus_words, [{attributes, record_info(fields, webgnosus_words)}, {disc_only_copies, [node()]}]).

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
        webgnosus_util:get_attribute(count, R),
        webgnosus_util:get_attribute(frequency, R),
        webgnosus_util:get_attribute(pos, R)
    });

write(_) ->
    error.

%% return row count
count() ->    
    webgnosus_dbi:count(webgnosus_words).

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
    webgnosus_dbi:q(qlc:q([X || X <- mnesia:table(webgnosus_words)]));

%% find all models where text matches specified rexp
find({word, R}) ->
    webgnosus_dbi:q(qlc:q([W || W <- mnesia:table(webgnosus_words), word_contains(W, R)]));

%% find specified record to database
find({Word}) ->
    case webgnosus_dbi:read_row({webgnosus_words, Word}) of
        [] ->
            error;
        Result ->
            hd(Result)
     end.

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
    webgnosus_util:values(Result);

%% return sorted list of most frequent words filtered by 
most_frequent([{word, RegExp}, {count, Count}]) ->      
    Result = webgnosus_dbi:fold(
        fun(W, Words) ->  
            larger_count(W, Words, Count)
        end, 
        [], 
        qlc:q([W || W <- mnesia:table(webgnosus_words), word_contains(W, RegExp)])),
    webgnosus_util:values(Result).

%%--------------------------------------------------------------------
%% Func: total_word_count/0
%% Description: find models
%%--------------------------------------------------------------------
%% return sorted list of most frequent words
total_word_count() ->      
    webgnosus_dbi:fold(
        fun(#webgnosus_words{count = WordCount}, Count) ->  
            Count + WordCount
        end, 
        0, 
        qlc:q([W || W <- mnesia:table(webgnosus_words)])).

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% attributes
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
word(#webgnosus_words{word = Attr}) ->    
    Attr.

count(#webgnosus_words{count = Attr}) ->    
    Attr.

frequency(#webgnosus_words{frequency = Attr}) ->    
    Attr.

pos(#webgnosus_words{pos = Attr}) ->    
    Attr.

%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%% text analysis
%%>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
%%--------------------------------------------------------------------
%% Func: count_words/1
%% Description: counts words in tokenized document
%%--------------------------------------------------------------------
count_words(Tokens) ->
    lists:foreach(        
        fun(T) ->
            case find({T}) of
                error ->
                    write([{word, T}, {count, 1}]);
                #webgnosus_words{count = Count, frequency = Frequency, pos = Pos} ->
                    write([{word, T}, {count, Count + 1}, {frequency, Frequency}, {pos, Pos}])                    
            end
        end,
        Tokens).

%%--------------------------------------------------------------------
%% Func: calculate_word_frequency/0
%% Description: determine word counts for status text
%%--------------------------------------------------------------------
% count words for all status messages and save
calculate_word_frequency() ->
    Total = total_word_count(),
    webgnosus_dbi:foreach(
        fun(#webgnosus_words{word = Word, count = Count, pos = Pos}) ->  
            write([{word, Word}, {count, Count}, {frequency, Count/Total}, {pos, Pos}])
        end, 
        webgnosus_words).


%%--------------------------------------------------------------------
%% Func: dump/2
%% Description: dump list of terms to specified file
%%--------------------------------------------------------------------
dump(File, {count, Count}) ->      
    webgnosus_util:dump(File, lists:reverse(most_frequent({count, Count})));

dump(File, [{word, RegExp}, {count, Count}]) ->      
    webgnosus_util:dump(File, lists:reverse(most_frequent([{word, RegExp}, {count, Count}]))).

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
%%--------------------------------------------------------------------
%% Func: later/3
%% Description: build list of words with largest count
%%--------------------------------------------------------------------
larger_count(#webgnosus_words{count = WordCount} = Word, Large, Count) ->
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

%%--------------------------------------------------------------------
%% Func: word_contains/2
%% Description: true if specified rexp matches status text.
%%--------------------------------------------------------------------
word_contains(#webgnosus_words{word = W}, R) ->
    case regexp:first_match(W, R) of
        {match, _, _} ->
            true;
        _ -> 
            false
    end.

