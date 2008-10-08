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
%% Func: remove new lines
%% Description: .
%%--------------------------------------------------------------------
remove_new_lines(Doc) ->    
    case regexp:gsub(Doc, "\\n", " ") of
        {ok, NewDoc, _} ->
            NewDoc;
        _ -> 
            Doc
    end.

%%--------------------------------------------------------------------
%% Func: split/1
%% Description: return list of document words.
%%--------------------------------------------------------------------
split(Doc) ->
    case regexp:split(Doc, "\s+") of
        {ok, NewDoc} ->
            NewDoc;
        X -> 
            X
    end.
  
