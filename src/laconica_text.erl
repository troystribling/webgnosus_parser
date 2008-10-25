%%%-------------------------------------------------------------------
%%% parser laconica response messages
%%%-------------------------------------------------------------------
-module(laconica_text).

%% API
-export([
          tokenize_and_resolve_urls/1,
          tokenize/1,
          prepare/1,
          is_language/2,
          get_tags/1,
          get_urls/1,
          dump_status_text/1
         ]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: tokenize_and_resolve_urls/1
%% Description: tokenize status text and resolve redirected URLs
%%--------------------------------------------------------------------
tokenize_and_resolve_urls(S) ->
   Toks =  webgnosus_text:tokenize(S),
   Urls = get_urls(Toks),
   MapUrls = lists:map(
        fun(U) ->  
            webgnosus_http:get_redirect_url(U)
        end, 
        Urls),
    lists:append(
        lists:subtract(Toks, Urls),
        MapUrls
    ).

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
%% Func: is_language/1
%% Description: return true if status is in specified language
%%--------------------------------------------------------------------
is_language(S, Dictionary) ->
   webgnosus_dictionary_model:is_language(S, Dictionary) and webgnosus_text:is_ascii(S).

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
%% Description: dump status text messages of specified lnaguage to file
%%--------------------------------------------------------------------
dump_status_text([{file, File}, {language, Language}]) -> 
    dump(
        fun (Fh, S, Dictionary) ->
            case is_language(S, Dictionary) of 
                true ->
                   io:format(Fh, "~p~n~n", [S]);
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

