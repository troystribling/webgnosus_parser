%%%-------------------------------------------------------------------
%%% parser laconica response messages
%%%-------------------------------------------------------------------
-module(laconica_text).

%% API
-export([
          tokenize/1,
          prepare/1,
          in_dictionary/2,
          get_tags/1,
          get_urls/1,
          dump_status_text/1
         ]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: tokenize/1
%% Description: tokenize status text and resolve redirected URLs
%%--------------------------------------------------------------------
tokenize(S) ->
   webgnosus_text:tokenize(S).

%%--------------------------------------------------------------------
%% Func: prepare/1
%% Description: tokenize status text and resolve redirected URLs
%%--------------------------------------------------------------------
prepare(S) ->
   webgnosus_text:prepare(S).

%%--------------------------------------------------------------------
%% Func: in_dictionary/1
%% Description: return true if status is in specified language
%%--------------------------------------------------------------------
in_dictionary(S, Dictionary) ->
   webgnosus_dictionary_model:in_dictionary([{document, S}, {dictionary, Dictionary}]) and webgnosus_text:is_ascii(S).

%%--------------------------------------------------------------------
%% Func: get_tags/1
%% Description: return all tags in status
%%--------------------------------------------------------------------
get_tags(Tokens) when is_list(Tokens) ->
    webgnosus_util:filter(Tokens, "^#").

%%--------------------------------------------------------------------
%% Func: get_urls/1
%% Description: return all urls in status
%%--------------------------------------------------------------------
get_urls(Tokens) when is_list(Tokens) ->
    webgnosus_util:filter(Tokens, "^http:").

%%--------------------------------------------------------------------
%% Func: dump_language/2
%% Description: dump status text messages of specified language to file
%%--------------------------------------------------------------------
dump_status_text([{file, File}, {language, Language}]) -> 
    dump(
        fun (Fh, S, Dictionary) ->
            case in_dictionary(S, Dictionary) of 
                true ->
                   io:format(Fh, "~p.~n", [S]);
                false ->
                    void
            end
        end,
        File,
        Language);

%% dump status text messages not of specified language to file
dump_status_text([negate, {file, File}, {language, Language}]) -> 
    dump(
        fun (Fh, S, Dictionary) ->
            case not in_dictionary(S, Dictionary) of 
                true ->
                   io:format(Fh, "~p.~n", [S]);
                false ->
                    void
            end
        end,
        File,
        Language);

%% dump status text messages niot of specified language to file
dump_status_text([prepare, {file, File}, {language, Language}]) -> 
    dump(
        fun (Fh, S, Dictionary) ->
            case in_dictionary(S, Dictionary) of 
                true ->
                   io:format(Fh, "~s~n", [prepare(webgnosus_text:replace_shortened_urls(S))]);
                false ->
                    void
            end
        end,
        File,
        Language).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: dump_language/2
%% Description: dump status text messages of specified lnaguage to file
%%--------------------------------------------------------------------
dump(F, File, Language) ->      
    Dictionary = webgnosus_dictionary_model:load_dictionary(Language),
    case file:open(File, write) of
        {ok, Fh} ->
            webgnosus_dbi:foreach(
                fun(S) ->  
                    LcS = webgnosus_text:to_lower(laconica_status_model:text(hd(S))),
                    F(Fh, LcS, Dictionary)
                end, 
                laconica_statuses);
        Error ->
            Error
    end.

