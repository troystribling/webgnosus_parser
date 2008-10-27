%%%-------------------------------------------------------------------
%%% webgnosus text processing utilities
%%%-------------------------------------------------------------------
-module(webgnosus_text).

%% API
-export([
          prepare/1,
          tokenize/1,
          to_lower/1,
          is_ascii/1,
          replace_shortened_urls/1
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
        pad_periods(
            pad_single_quotes(
                pad_document(
                    remove_new_lines(
                        remove_smiley(Doc)))))).

%%--------------------------------------------------------------------
%% Func: prepare/1
%% Description: prepare text for processing.
%%--------------------------------------------------------------------
to_lower(Doc) ->
    string:to_lower(Doc).

%%--------------------------------------------------------------------
%% Func: is_ascii/1
%% Description: prepare text for processing.
%%--------------------------------------------------------------------
is_ascii(Doc) ->
    lists:all(
        fun(C) ->
            C < 128
        end,
        Doc).

%%--------------------------------------------------------------------
%% Func: replace_shortened_urls/1
%% Description: resolve shortened urls and replace with full url
%%--------------------------------------------------------------------
replace_shortened_urls(Doc) ->
    case regexp:matches(Doc,  "http://[a-z,A-Z,0-9,\.\?/=&_\,\+,\-]+") of
        {match, []} ->
            Doc;
        {match, Matches} ->
            get_url_and_update_document(Matches, Doc);
        _ -> 
            Doc
    end.

get_url_and_update_document([], Doc) ->
        Doc;

get_url_and_update_document([{Position, Length} = M| Matches], Doc) ->
    OldUrl = lists:sublist(Doc, Position, Length),
    NewUrl = webgnosus_http:get_redirect_url(OldUrl),
    get_url_and_update_document(
        lists:map(fun({P,L}) -> {P + length(NewUrl) - length(OldUrl), L} end, Matches), 
        webgnosus_util:replace_at_position(M, NewUrl, Doc)).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: pad_period/1
%% Description: remove smileys from document
%%--------------------------------------------------------------------
pad_periods(Doc) ->    
    case regexp:matches(Doc, "[0-9a-zA-Z]+\\.\\s") of
        {match, Matches} ->
            {Result, _} = lists:foldl(
                fun({Pos, Length}, {D, C}) ->
                    {webgnosus_util:replace_at_position({Pos + Length - 2 + C, 2}, " . ", D), C+ 1}
                end,
                {Doc, 0},
                Matches),
            Result;
        _ -> 
            Doc
    end.

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
%% Func: remove_smiley/1
%% Description: remove smileys.
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
%% Func: pad_document/1
%% Description: pad document
%%--------------------------------------------------------------------
pad_document(Doc) ->   
    lists:concat([" ", Doc, " "]).

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
        webgnosus_util:replace_at_position(Lh, " ' ", webgnosus_util:replace_at_position(Rh, " ' ", Doc))).

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
    case regexp:split(string:strip(Doc), "\s+") of
        {ok, NewDoc} ->
            NewDoc;
        X -> 
            X
    end.
  