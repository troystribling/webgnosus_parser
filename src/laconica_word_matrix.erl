%%%-------------------------------------------------------------------
%%% parser laconica response messages
%%%-------------------------------------------------------------------
-module(laconica_word_matrix).

%% API
-export([
        ]).

%% include
-include_lib("laconica_model.hrl").

%%====================================================================
%% API
%%====================================================================

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: status/2
%% Description: extract status data from status records in xml 
%%              document.
%%--------------------------------------------------------------------