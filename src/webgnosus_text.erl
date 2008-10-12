%%%-------------------------------------------------------------------
%%% webgnosus text processing utilities
%%%-------------------------------------------------------------------
-module(webgnosus_text).

%% API
-compile(export_all).

-export([
          prepare/1
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: split/1
%% Description: prepare text for processing.
%%--------------------------------------------------------------------
prepare(Doc) ->
    remove_new_lines(pad_punctuation(Doc)).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: pad_punctuation/1
%% Description: place spaces before and after puctutaion for post 
%%              processing for all punctuation symbols.
%%--------------------------------------------------------------------
pad_punctuation(Doc) ->    
    lists:foldl(
        fun(P, D) -> 
            pad_punctuation(P, D) 
        end, 
        Doc, 
        webgnosus_punctuation_model:find(punctuation)).

%%--------------------------------------------------------------------
%% Func: pad_punctuation/2
%% Description: place spaces before and after puctutaion for post 
%%              processing.
%%--------------------------------------------------------------------
pad_punctuation(P, Doc) -> 
io:format("~p~n", [webgnosus_punctuation_model:regexp(P)]),   
io:format("~p~n", [webgnosus_punctuation_model:word(P)]),   
    case regexp:gsub(Doc, webgnosus_punctuation_model:regexp(P), " " ++ webgnosus_punctuation_model:word(P) ++ " ") of
        {ok, NewDoc, _} ->
            NewDoc;
        _ -> 
            Doc
    end.

%%--------------------------------------------------------------------
%% Func: remove_smiley/2
%% Description: remove smileys from document
%%--------------------------------------------------------------------
remove_smiley(Smiley, Doc) ->    
    case regexp:gsub(Doc, Smiley, "") of
        {ok, NewDoc, _} ->
            NewDoc;
        _ -> 
            Doc
    end.

%%--------------------------------------------------------------------
%% Func: replace_at_position/3
%% Description: remove smileys from document
%%--------------------------------------------------------------------
replace_at_position({Pos, Length}, Rep, Doc) ->    
    {Head, Tail} = lists:split(Pos - 1, Doc),
    NewTail = lists:sublist(Tail, Length + 1, length(Tail) - Length),
    lists:concat([Head, Rep, NewTail]).


%%--------------------------------------------------------------------
%% Func: pad_single_quotes/1
%% Description: disambiguate single quotes and
%%              apostraphes and pad single quotes with spaces.
%%--------------------------------------------------------------------
pad_single_quotes(Doc) ->   
    {_, Right} = regexp:matches(Doc, "\\'\\s|\\'$"),
    {_, Left}  = regexp:matches(Doc, "\\s\\'|^\\'"),
    find_single_quote_pairs_and_pad(Right, Left, Doc).

%%--------------------------------------------------------------------
%% Func: pad_single_quote_pairs/3
%% Description: match single quote pairs and pad with spaces.
%%--------------------------------------------------------------------
find_single_quote_pairs_and_pad([], [], Doc) ->   
    Doc;

find_single_quote_pairs_and_pad(_Right, [], Doc) ->   
    Doc;

find_single_quote_pairs_and_pad([], _Left, Doc) ->   
    Doc;

find_single_quote_pairs_and_pad([Rh | Rt] = Right, [Lh | Lt] = Left, Doc) ->   
    case is_apostrophe(Rh, Lh, Lt)  of
        right  ->
            find_single_quote_pairs_and_pad(Rt, Left, Doc);
        left   -> 
            find_single_quote_pairs_and_pad(Right, Lt, Doc);            
        quotes ->
            find_single_quote_pairs_and_pad(
                update_single_quote_match_position(Rt), 
                update_single_quote_match_position(Lt), 
                replace_at_position(Lh, " ' ", replace_at_position(Rh, " ' ", Doc)))
    end.
    

%%--------------------------------------------------------------------
%% Func: update_single_quote_match_position/1
%% Description: 
%%--------------------------------------------------------------------
update_single_quote_match_position(PosList) ->
    lists:map(
        fun({Pos, Length}) -> 
            {Pos + 2, Length}                 
        end, 
        PosList).

%%--------------------------------------------------------------------
%% Func: is_apostrophe/3
%% Description: determine apostraphe type or quotes .
%%--------------------------------------------------------------------
is_apostrophe({RhPos, _}, {LhPos, _}, []) ->   
    if
        RhPos < LhPos -> 
            right;
        true -> 
            quotes
    end;

is_apostrophe({RhPos, _}, {LhPos, _}, [{Lh2Pos, _} | _]) ->   
    if
        RhPos < LhPos -> 
            right;
        Lh2Pos < RhPos -> 
            left;
        true ->
            quotes
    end.

%%--------------------------------------------------------------------
%% Func: remove_new_lines/1
%% Description: remove new line chracters from document.
%%--------------------------------------------------------------------
remove_new_lines(Doc) ->    
    case regexp:gsub(Doc, "\\n", " ") of
        {ok, NewDoc, _} ->
            NewDoc;
        _ -> 
            Doc
    end.

%%--------------------------------------------------------------------
%% Func: tokens/1
%% Description: return list of document words and puctuation.
%%--------------------------------------------------------------------
tokens(Doc) ->
    case regexp:split(Doc, "\s+") of
        {ok, NewDoc} ->
            NewDoc;
        X -> 
            X
    end.
  
