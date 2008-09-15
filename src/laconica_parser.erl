%%%-------------------------------------------------------------------
%%% parser laconica response messages
%%%-------------------------------------------------------------------
-module(laconica_parser).

%% API
-export([
          statuses/1
        ]).

%% include
-include_lib("laconica_model.hrl").
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

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: status(Node) -> Result
%% Description: extract data from records in xml document with 
%%              tag status.
%%--------------------------------------------------------------------
status(Node) ->
    Status = #laconica_statuses{
        created_at            = extract_text("/status/created_at/text()", Node),
        status_id             = extract_text("/status/id/text()", Node),
        text                  = extract_text("/status/text/text()", Node),
        source                = extract_text("/status/source/text()", Node),
        truncated             = extract_text("/status/truncated/text()", Node),
        in_reply_to_status_id = extract_text("/status/in_reply_to_status_id/text()", Node),
        in_reply_to_user_id   = extract_text("/status/in_reply_to_user_id/text()", Node),
        favorited             = extract_text("/status/favorited/text()", Node)
    },
    case xmerl_xpath:string("/status/user", Node) of
        [] -> Status;
        [UserNode] -> Status#laconica_statuses{user_id = extract_text("/user/id/text()", UserNode)}
    end.
    
%%--------------------------------------------------------------------
%% Func: extract_text(Body) -> Result
%% Description: extract text field from xmlText object
%%--------------------------------------------------------------------
extract_text(Xpath, Xml) ->
    lists:foldr(
        fun(#xmlText{value = Val}, Acc) -> [Val|Acc];
           (_, Acc) -> Acc
        end,
        "",
        xmerl_xpath:string(Xpath, Xml)
    ).

