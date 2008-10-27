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
    get_redirect_url(Url, 0).

get_redirect_url(Url, Count) ->
    if
        Count < 5 ->
            case valid_url(Url) of
                true ->
                    extract_redirect_url_from_header(Url, Count);
                false ->
                     Url
            end;
        true ->
            Url
    end.

extract_redirect_url_from_header(Url, Count) ->
    HttpDoc = http:request(get, {Url, headers()}, [{autoredirect, false}, {timeout, 15000}], []),
    case HttpDoc of
        {ok, {{_, 301 ,_}, Headers, _}} -> 
            get_redirect_url(webgnosus_util:get_attribute("location", Headers), Count + 1);
        {ok, {{_, 302 ,_}, Headers, _}} -> 
            get_redirect_url(webgnosus_util:get_attribute("location", Headers), Count + 1);
        {ok, {{_, 303 ,_}, Headers, _}} -> 
            get_redirect_url(webgnosus_util:get_attribute("location", Headers), Count + 1);
        _ -> 
            Url
    end.

valid_url(Url) ->
    case regexp:first_match(Url, "^http://$|\\s") of
        {match, _, _} -> 
            false;
        _ -> 
            lists:all(fun(X) -> X < 128 end, Url)
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
  