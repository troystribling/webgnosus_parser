%%%-------------------------------------------------------------------
%%% webgnosus http utilities
%%%-------------------------------------------------------------------
-module(webgnosus_http).

%% API
-export([
          get_url/1,
          parse_xml/1
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: get_url(Url) -> Result
%% Description: http get request to specified url.
%%--------------------------------------------------------------------
get_url(Url) ->
    HTTPDoc = http:request(get, {Url, headers()}, [], []),
    case HTTPDoc of
        {ok, {_Status, _Headers, Body}} -> Body;
        _ -> {error}
    end.

%%--------------------------------------------------------------------
%% Func: headers() -> Result
%% Description: build request headers.
%%--------------------------------------------------------------------
headers() -> [{"User-Agent", "Webgnos.us/0.0"}].

%%--------------------------------------------------------------------
%% Func: parse_xml(Body) -> Result
%% Description: parse xml document
%%--------------------------------------------------------------------
parse_xml(Document) ->
    try xmerl_scan:string(Document) of 
        Result ->
            {Xml, _Rest} = Result, 
            Xml
    catch
        {'EXIT', _} -> {error};
        {error, _} -> {error}
    end.

%%====================================================================
%%% Internal functions
%%====================================================================
