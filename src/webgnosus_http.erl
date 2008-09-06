%%%-------------------------------------------------------------------
%%% webgnosus http utilities
%%%-------------------------------------------------------------------
-module(webgnosus_http).

%% API
-export([
         build_url/2,
         get_url/1
        ]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: get_url(Url) -> Result
%% Description: http get request to specified url.
%%--------------------------------------------------------------------
get_url(Url) ->
    HTTPDoc = http:request(get, {Url, headers(Login, Pass)}, [], []),
    case HTTPDoc of
        {ok, {_Status, _Headers, Body}} -> Body;
        _ -> {error}
    end;

%%--------------------------------------------------------------------
%% Func: build_url(Url, Args) -> Result
%% Description: build request url.
%%--------------------------------------------------------------------
build_url(Url, []) -> Url;
build_url(Url, Args) ->
    ArgStr = lists:concat(
        lists:foldl(
            fun (Rec, []) -> [Rec];
                (Rec, Ac) -> [Rec, "&" | Ac]
            end,
            [],
            [K ++ "=" ++ yaws_api:url_encode(V) || {K, V} <- Args]
        )
    ),
    Url ++ "?" ++ ArgStr.

%%====================================================================
%%% Internal functions
%%====================================================================
