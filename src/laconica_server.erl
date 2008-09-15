%%%-------------------------------------------------------------------
%%% File        : laconica_interface
%%% Description : fetch data from laconica microbolgging servers and pass to 
%%%               analysis servers
%%%-------------------------------------------------------------------
-module(laconica_server).

-behaviour(gen_server).

%% API
-export([
         start_link/0,
         public_timeline/0,
         open_session/1,
         open_session/2
        ]).

%% gen_server callbacks
-export([
         init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
	 terminate/2, 
         code_change/3
        ]).

%% include
-include_lib("laconica_model.hrl").

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    webgnosus_events:message({started, ?MODULE}),
    {ok, gb_trees:empty()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% open session with specified Url
handle_call({open_session, Url, PollFrequency}, _From, Sessions) ->  
    {Status, Pid} = do_open_session(Url, PollFrequency),
    case Status of
        ok -> {reply, Status, add_session(Url, Pid, Sessions)};
        _  -> {reply, Status, Sessions}
    end.

%%--------------------------------------------------------------------
%% open session with specified Url
handle_call(public_timeline, _From, Sessions) ->  
    {reply, do_public_timeline(Sessions), Sessions}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    webgnosus_events:message({stopped, ?MODULE}),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Func: public_timeline() -> Result
%% Description: request public timeline from server
%%--------------------------------------------------------------------
public_timeline() ->
    gen_server:call(?MODULE, public_timeline).

%%--------------------------------------------------------------------
%% Func: open_session(Url) -> Result
%% Description: open session to laconica server at Url and set poll
%%              frequency of public timeline to 1 second
%%--------------------------------------------------------------------
open_session(Url) ->
    gen_server:call(?MODULE, {open_session, Url, 1}).

%%--------------------------------------------------------------------
%% Func: open_session(Url, Poll_frequency) -> Result
%% Description: request public timeline from server, set poll
%%              default to 1 second
%%--------------------------------------------------------------------
open_session(Url, PollFrequency) ->
    gen_server:call(?MODULE, {open_session, Url, PollFrequency}).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: do_open_session() -> Result
%% Description: spawn laconica server interface process
%%--------------------------------------------------------------------
do_open_session(Url, PollFrequency) ->
    laconica_site_model:write(#laconica_site{root_url = Url, poll_frequency = PollFrequency}),
    laconica_interface:start_link(Url).

%%--------------------------------------------------------------------
%% Func: do_public_timeline(Sessions) -> Result
%% Description: spawn laconica server interface process
%%--------------------------------------------------------------------
do_public_timeline(Sessions) ->
    SessionList = gb_trees:to_list(Sessions),
    case length(SessionList) of
      0 -> webgnosus_events:warning(["open_session before retrieving public timeline."]), error;
      _ -> [gen_server:call(Pid, public_timeline)|| {_Url, Pid} <- SessionList]
    end.
      
%%--------------------------------------------------------------------
%% Func: add_session(Sessions) -> Result
%% Description: add session to session hash
%%--------------------------------------------------------------------
add_session(Url, Pid, Sessions) ->
    case gb_trees:is_defined(Url, Sessions) of
        true -> Sessions;
        false -> gb_trees:insert(Url, Pid, Sessions)
    end.
