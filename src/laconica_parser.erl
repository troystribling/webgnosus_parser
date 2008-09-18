%%%-------------------------------------------------------------------
%%% parser laconica response messages
%%%-------------------------------------------------------------------
-module(laconica_parser).

%% API
-export([
          statuses/2,
          status_users/2
        ]).

%% include
-include_lib("laconica_model.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Func: statuses(Body) -> Result
%% Description: extract status message.
%%--------------------------------------------------------------------
statuses(Body, SiteUrl) ->
    [status(Node, SiteUrl) || Node <- lists:flatten([xmerl_xpath:string("/statuses/status", Body)])].

%%--------------------------------------------------------------------
%% Func: statuses(Body) -> Result
%% Description: extract user information from status message.
%%--------------------------------------------------------------------
status_users(Body, SiteUrl) ->
    [status_user(Node, SiteUrl) || Node <- lists:flatten([xmerl_xpath:string("/statuses/status", Body)])].

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: status/2
%% Description: extract status data from status records in xml 
%%              document.
%%--------------------------------------------------------------------
status(Node, SiteUrl) ->
    StatusId = extract_text("/status/id/text()", Node),
    Status = #laconica_statuses{
        created_at            = extract_text("/status/created_at/text()", Node),
        status_id             = StatusId,
        site                  = SiteUrl,
        text                  = extract_text("/status/text/text()", Node),
        source                = extract_text("/status/source/text()", Node),
        truncated             = extract_text("/status/truncated/text()", Node),
        in_reply_to_status_id = extract_text("/status/in_reply_to_status_id/text()", Node),
        in_reply_to_user_id   = extract_text("/status/in_reply_to_user_id/text()", Node),
        favorited             = extract_text("/status/favorited/text()", Node)
    },
    case xmerl_xpath:string("/status/user", Node) of
        [] -> Status;
        [UserNode] -> 
            UserId = extract_text("/user/id/text()", UserNode),
            Status#laconica_statuses{
                user_id   = UserId,
                user_name = extract_text("/user/name/text()", UserNode),
                status_id = {StatusId, UserId, SiteUrl}
            }
    end.
    
%%--------------------------------------------------------------------
%% Func: status_user/2
%% Description: extract user data from status records in xml document with 
%%              tag status.
%%--------------------------------------------------------------------
status_user(Node, SiteUrl) ->
    [UserNode] = xmerl_xpath:string("/status/user", Node),
    #laconica_users{
        user_id           = {extract_text("/user/id/text()", UserNode), SiteUrl},
        user_name         = extract_text("/user/name/text()", UserNode),
        followers_count   = extract_text("/user/followers_count/text()", UserNode),
        screen_name       = extract_text("/user/screen_name/text()", UserNode),
        description       = extract_text("/user/description/text()", UserNode),
        location          = extract_text("/user/location/text()", UserNode),
        profile_image_url = extract_text("/user/profile_image_url/text()", UserNode),
        protected         = extract_text("/user/protected/text()", UserNode),
        url               = extract_text("/user/url/text()", UserNode)
    }.
    
%%--------------------------------------------------------------------
%% Func: extract_text/1
%% Description: extract text field from xmlText object
%%--------------------------------------------------------------------
extract_text(Xpath, Xml) ->
    Text = lists:foldr(
        fun(#xmlText{value = V}, A) -> [V|A];
                             (_, A) -> A
        end,
        "",
        xmerl_xpath:string(Xpath, Xml)
    ),
    case Text of
        [T] -> T;
         _  -> lists:concat(Text)
    end.
