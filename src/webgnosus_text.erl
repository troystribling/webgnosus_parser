%%%-------------------------------------------------------------------
%%% webgnosus text processing utilities
%%%-------------------------------------------------------------------
-module(webgnosus_text).

%% API
-export([
          prepare/1,
          tokenize/1
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: prepare/1
%% Description: prepare text for processing.
%%--------------------------------------------------------------------
tokenize(Doc) ->
    tokens(prepare(Doc)).

%%--------------------------------------------------------------------
%% Func: prepare/1
%% Description: prepare text for processing.
%%--------------------------------------------------------------------
prepare(Doc) ->
    pad_punctuation(
        pad_single_quotes(
            remove_new_lines(
                remove_smiley(Doc)))).

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
    case regexp:gsub(Doc, webgnosus_punctuation_model:regexp(P), " " ++ webgnosus_punctuation_model:word(P) ++ " ") of
        {ok, NewDoc, _} ->
            NewDoc;
        _ -> 
            Doc
    end.

%%--------------------------------------------------------------------
%% Func: pad_punctuation/1
%% Description: place spaces before and after puctutaion for post 
%%              processing for all punctuation symbols.
%%--------------------------------------------------------------------
remove_smiley(Doc) ->    
    lists:foldl(
        fun(P, D) -> 
            remove_smiley(P, D) 
        end, 
        Doc, 
        webgnosus_punctuation_model:find(smiley)).

%%--------------------------------------------------------------------
%% Func: remove_smiley/2
%% Description: remove smileys from document
%%--------------------------------------------------------------------
remove_smiley(S, Doc) ->    
    case regexp:gsub(Doc, webgnosus_punctuation_model:regexp(S), "") of
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
    PaddedDoc = lists:concat([" ", Doc, " "]),
    {_, Right} = regexp:matches(PaddedDoc, "\\'\\s"),
    {_, Left}  = regexp:matches(PaddedDoc, "\\s\\'"),
    find_single_quote_pairs_and_pad(Right, Left, PaddedDoc).

%%--------------------------------------------------------------------
find_single_quote_pairs_and_pad([], [], Doc) ->   
    Doc;

find_single_quote_pairs_and_pad(_Right, [], Doc) ->   
    Doc;

find_single_quote_pairs_and_pad([], _Left, Doc) ->   
    Doc;

find_single_quote_pairs_and_pad(Right, Left, Doc) ->   
    if 
        length(Right) == length(Left) ->
            pad_matched_single_quote_pairs(Right, Left, Doc);
        true ->
            pad_mismatched_single_quote_pairs(Right, Left, Doc)
    end.
  
%%--------------------------------------------------------------------
pad_mismatched_single_quote_pairs([Rh | Rt] = Right, [Lh | Lt] = Left, Doc) ->   
    case is_apostrophe(Rh, Lh, Lt)  of
        right  ->
            find_single_quote_pairs_and_pad(Rt, Left, Doc);
        left   -> 
            find_single_quote_pairs_and_pad(Right, Lt, Doc);            
        quotes ->
            pad_matched_single_quote_pairs(Right, Left, Doc)
    end.

%%--------------------------------------------------------------------
pad_matched_single_quote_pairs([Rh | Rt], [Lh | Lt], Doc) ->   
    find_single_quote_pairs_and_pad(
        update_right_single_quote_match_position(Rt), 
        update_left_single_quote_match_position(Lt, Rh), 
        replace_at_position(Lh, " ' ", replace_at_position(Rh, " ' ", Doc))).

%%--------------------------------------------------------------------
update_right_single_quote_match_position(PosList) ->
    update_single_quote_match_position(PosList, 2).

update_left_single_quote_match_position([], _) ->
     [];

update_left_single_quote_match_position([{Lh2Pos, _} | _] = PosList, {RhPos, _}) ->
    if
        Lh2Pos < RhPos -> 
            update_single_quote_match_position(PosList, 1);
        true ->
            update_single_quote_match_position(PosList, 2)
    end.

update_single_quote_match_position(PosList, OffSet) ->
    lists:map(
        fun({Pos, Length}) -> 
            {Pos + OffSet, Length}                 
        end, 
        PosList).

%%--------------------------------------------------------------------
is_apostrophe({RhPos, _}, {LhPos, _}, []) ->   
    if
        RhPos < LhPos -> 
            right;
        true -> 
            quotes
    end;

%%--------------------------------------------------------------------
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
    case regexp:split(string:to_lower(string:strip(Doc)), "\s+") of
        {ok, NewDoc} ->
            NewDoc;
        X -> 
            X
    end.
  
