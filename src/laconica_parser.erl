%%%-------------------------------------------------------------------
%%% parser laconica response messages
%%%-------------------------------------------------------------------
-module(laconica_parser).

%% API
-export([
          statuses/1
        ]).

%% include
-include_lib("laconica.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: statuses(Body) -> Result
%% Description: extract data from records in xml document with 
%%              tag statuses.
%%--------------------------------------------------------------------
statuses(Body) ->
    [status(Node) || Node <- lists:flatten([xmerl_xpath:string("/statuses/status", Body)])].

%%--------------------------------------------------------------------
%% Func: status(Node) -> Result
%% Description: extract data from records in xml document with 
%%              tag status.
%%--------------------------------------------------------------------
status(Node) ->
    Node.

%%--------------------------------------------------------------------
%% Func: extract_text(Body) -> Result
%% Description: extract text field from xmlText object
%%--------------------------------------------------------------------
extract_text(Xml, Xpath) ->
    lists:foldr(
        fun(#xmlText{value = Val}, Acc) -> lists:append(Val, Acc);
           (_, Acc) -> Acc
        end,
        "",
        xmerl_xpath:string(Xpath, Xml)
    ).

%%====================================================================
%%% Internal functions
%%====================================================================
