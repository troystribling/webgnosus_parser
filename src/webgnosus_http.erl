%%%-------------------------------------------------------------------
%%% webgnosus http utilities
%%%-------------------------------------------------------------------
-module(webgnosus_http).

%% API
-export([
          get_url/1,
          get_redirect_url/1,
          is_live_url/1,
          parse_xml/1
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: get_url/1
%% Description: http get request to specified url.
%%--------------------------------------------------------------------
get_url(Url) ->
    HttpDoc = http:request(get, {Url, headers()}, [], []),
    case HttpDoc of
        {ok, {{_,200,_}, _Headers, Body}} -> 
            Body;
        _ -> 
            {error}
    end.

%%--------------------------------------------------------------------
%% Func: is_live_url/1
%% Description: returns true if 200 is returned false otherwise.
%%--------------------------------------------------------------------
is_live_url(Url) ->
    HttpDoc = http:request(get, {Url, headers()}, [], []),
    case HttpDoc of
        {ok, {{_,200,_}, _, _}} -> 
            true;
        _ -> 
            false
    end.


%%--------------------------------------------------------------------
%% Func: get_redirect_url/1
%% Description: return redirect URL.
%%--------------------------------------------------------------------
get_redirect_url(Url) ->
io:format("~p~n", [Url]),   
    case valid_url(Url) of
        true ->
            extract_redirect_url_from_header(Url);
        false ->
             Url
    end.

%%--------------------------------------------------------------------
%% Func: headers/0
%% Description: build request headers.
%%--------------------------------------------------------------------
headers() -> 
    [{"User-Agent", "web.gnos.us/0.0"}].

%%--------------------------------------------------------------------
%% Func: parse_xml/1
%% Description: parse xml document
%%--------------------------------------------------------------------
parse_xml(Document) ->
    try xmerl_scan:string(Document) of 
        Result ->
            {Xml, _Rest} = Result, 
            Xml
    catch
        {'EXIT', _} -> {error};
        {error, _}  -> {error}
    end.

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: extract_redirect_url_from_header/1
%% Description: return redirect URL.
%%--------------------------------------------------------------------
extract_redirect_url_from_header(Url) ->
    HttpDoc = http:request(get, {Url, headers()}, [{autoredirect, false}, {timeout, 15000}], []),
    case HttpDoc of
        {ok, {{_, 301 ,_}, Headers, _}} -> 
            webgnosus_util:get_attribute("location", Headers);
        _ -> 
            Url
    end.

%%--------------------------------------------------------------------
%% Func: valid_url/1
%% Description: true if url is valid.
%%--------------------------------------------------------------------
valid_url(Url) ->
    case regexp:first_match(Url, "^http://$") of
        {match, _, _} -> 
            false;
        _ -> 
            is_ascii(Url)
    end.
 
is_ascii(Url) ->
    lists:all(fun(X) -> X < 128 end, Url).
 