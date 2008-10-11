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
%% Description: disambiguate single quotes and
%%              apostraphes and pad single quotes with spaces.
%%--------------------------------------------------------------------
pad_single_quotes(Doc) ->   
    {_, Right} = regexp:matches(Doc, "\\'\\s"),
    {_, Left}  = regexp:matches(Doc, "\\s\\'"),
    match_single_quote_pairs(Right, Left, Doc).

%%--------------------------------------------------------------------
%% Func: match_single_quote_pairs/3
%% Description: match single quote pairs and pad with spaces.
%%--------------------------------------------------------------------
match_single_quote_pairs([], [], Doc) ->   
    Doc;

match_single_quote_pairs(_Right, [], Doc) ->   
    Doc;

match_single_quote_pairs([], _Left, Doc) ->   
    Doc;

match_single_quote_pairs(Right, Left, Doc) ->   
    [Rh | Rt] = Right,
    [Lh | Lt]  = Left,
    if 
        is_right_apostrophe(Rh, Lh) ->
            match_single_quote_pairs(Rt, Left, Doc);
        is_left_apostrophe(Rh, Lt) ->
            match_single_quote_pairs(Right, Lt, Doc);            
        true    ->
            match_single_quote_pairs(Rt, Lt, replace_at_position(Lh, " ' ", replace_at_position(Rh, " ' ", Doc)))
    end.
    

%%--------------------------------------------------------------------
%% Func: is_right_apostrophe/2
%% Description: true if Rh is an apostrophe.
%%--------------------------------------------------------------------
is_right_apostrophe({RhPos, _}, {LhPos, _}) ->   
    if
        RhPos < LhPos -> 
            true;
        true -> 
            false
    end.

%%--------------------------------------------------------------------
%% Func: is_left_apostrophe/3
%% Description: true if Lh is an apostrophe.
%%--------------------------------------------------------------------
is_left_apostrophe(_Rh, []) ->   
    false;

is_left_apostrophe({RhPos, _}, [{Lh2Pos} | _]) ->   
    if
        Lh2Pos < RhPos -> 
            true;
        true -> 
            false
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
  
