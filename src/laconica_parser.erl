%%%-------------------------------------------------------------------
%%% parser laconica response messages
%%%-------------------------------------------------------------------
-module(laconica_parser).

%% API
-export([
          statuses/1
        ]).

%% include
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
%% Func: statuses(Body) -> Result
%% Description: extract data from records in xml document with 
%%              statuses tag
%%--------------------------------------------------------------------
text_or_default(_, [], Default) -> Default;

text_or_default(Xml, [Xpath | Tail], Default) ->
    Res = lists:foldr(
        fun(#xmlText{value = Val}, Acc) -> lists:append(Val, Acc);
           (_, Acc) -> Acc
        end,
        Default,
        xmerl_xpath:string(Xpath, Xml)
    ),
    text_or_default(Xml, Tail, Res).

%%====================================================================
%%% Internal functions
%%====================================================================
