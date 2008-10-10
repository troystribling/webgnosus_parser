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
    remove_new_lines(
        lists:foldl(
            fun(P, D) -> 
                pad_punctuation(P, D) 
            end, 
            Doc, 
            webgnosis_dictionary_model:find(punctuation))).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: pad_punctuation/1
%% Description: place spaces before and after puctutaion for post 
%%              processing.
%%--------------------------------------------------------------------
pad_punctuation(Punc, Doc) ->    
    case regexp:gsub(Doc, Punc, " " ++ Punc ++ " ") of
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
%% Description: must determine difference between single quotes and
%%              apostraphes.
%%--------------------------------------------------------------------
pad_single_quotes(Doc) ->   
    {_, Right} = regexp:matches(Doc, "\\'\\s"),
    {_, Left}  = regexp:matches(Doc, "\\s\\'"),
    match_single_quote_pairs(Right, Left, Doc).

%%--------------------------------------------------------------------
%% Func: match_single_quote_pairs/3
%% Description: match single quote pairs and pad with spaces.
%%--------------------------------------------------------------------
match_single_quote_pairs(Right, Left, Doc) ->   
    {_, Right} = regexp:matches(Doc, "\\'\\s"),
    {_, Left}  = regexp:matches(Doc, "\\s\\'"),
    match_single_quote_pairs(Right, Left, Doc).


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
  
