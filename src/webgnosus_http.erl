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
    HttpDoc = http:request(get, {Url, headers()}, [{autoredirect, false}], []),
    case HttpDoc of
        {ok, {{_,301,_}, Headers, _}} -> 
            get_location_from_headers(Headers);
        _ -> 
            {error}
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
%% Func: get_location_from_headers/1
%% Description: get location from 301 header.
%%--------------------------------------------------------------------
get_location_from_headers(Headers) ->
    case lists:keysearch("location", 1, Headers) of
        {value, {"location", Location}} ->
            Location;
        _ ->
            error
    end.
