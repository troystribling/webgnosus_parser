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
         open_session/2,
         close_session/1,
         stop_collectors/0,
         start_collectors/0
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
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    webgnosus_events:message({started, {?MODULE, self()}}),
    {ok, spawn_sessions(laconica_site_model:find(all), gb_trees:empty())}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout}     |
%%                                      {noreply, State}                   |
%%                                      {noreply, State, Timeout}          |
%%                                      {stop, Reason, Reply, State}       |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% open session with specified Url and poll frequency
handle_call({open_session, Url, PollFrequency}, _From, Sessions) ->  
    do_open_session(Url, PollFrequency, Sessions);

%% open session with specified Url and no poll frequency
handle_call({open_session, Url}, _From, Sessions) ->  
    do_open_session(Url, 0, Sessions);

%%--------------------------------------------------------------------
%% close session with specified Url
handle_call({close_session, Url}, _From, Sessions) ->  
    do_close_session(Url, Sessions);

%%--------------------------------------------------------------------
%% stop collectors
handle_call(stop_collectors, _From, Sessions) ->  
    do_collectors_request(stop_collection, Sessions);

%% start collectors
handle_call(start_collectors, _From, Sessions) ->  
    do_collectors_request(start_collection, Sessions);

%%--------------------------------------------------------------------
%% get public timeline
handle_call(public_timeline, _From, Sessions) ->  
    {reply, do_public_timeline(Sessions), Sessions}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State}          |
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
terminate(Reason, _State) ->
    webgnosus_events:message({stopped, {?MODULE, self(), Reason}}),
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
%% Func: public_timeline/0
%% Description: request public timeline from server
%%--------------------------------------------------------------------
public_timeline() ->
    gen_server:call(?MODULE, public_timeline).

%%--------------------------------------------------------------------
%% Func: open_session/1
%% Description: open session to laconica server at Url and set poll
%%              frequency of public timeline to 1 second
%%--------------------------------------------------------------------
open_session(Url) ->
    gen_server:call(?MODULE, {open_session, Url}).

%%--------------------------------------------------------------------
%% Func: open_session/2
%% Description: request public timeline from server and do not 
%%              poll server
%%--------------------------------------------------------------------
open_session(Url, PollFrequency) ->
    gen_server:call(?MODULE, {open_session, Url, PollFrequency}).

%%--------------------------------------------------------------------
%% Func: close_session/1
%% Description: close session to laconica server at Url 
%%--------------------------------------------------------------------
close_session(Url) ->
    gen_server:call(?MODULE, {close_session, Url}).

%%--------------------------------------------------------------------
%% Func: start_collectors/0
%% Description: start collectors 
%%--------------------------------------------------------------------
start_collectors() ->
    gen_server:call(?MODULE, start_collectors).

%%--------------------------------------------------------------------
%% Func: stop_collectors/0
%% Description: stop collectors 
%%--------------------------------------------------------------------
stop_collectors() ->
    gen_server:call(?MODULE, stop_collectors).

%%====================================================================
%%% API Handlers
%%====================================================================
%%--------------------------------------------------------------------
%% Func: do_public_timeline/1
%% Description: spawn laconica server interface process
%%--------------------------------------------------------------------
do_public_timeline(Sessions) ->
    SessionList = gb_trees:to_list(Sessions),
    case length(SessionList) of
      0 -> webgnosus_events:warning(["call open_session before retrieving public timeline."]), error;
      _ -> [gen_server:call(InterfacePid, public_timeline)|| {_Url, {InterfacePid, _CollectorPid}} <- SessionList]
    end.

%%--------------------------------------------------------------------
%% Func: do_open_session/3
%% Description: spawn laconica server interface process
%%--------------------------------------------------------------------
do_open_session(Url, PollFrequency, Sessions) ->
    case gb_trees:is_defined(Url, Sessions) of
        false ->
            Response = spawn_session(Url, PollFrequency, Sessions),
            laconica_site_model:write(#laconica_sites{root_url = Url, poll_frequency = PollFrequency}),
            Response;
        true -> 
            webgnosus_events:warning({session_open, Url}), 
            {reply, ok, Sessions}
    end.
                
%%--------------------------------------------------------------------
%% Func: do_close_session/2
%% Description: stop laconica server interface process
%%--------------------------------------------------------------------
do_close_session(Url, Sessions) ->
    case gb_trees:is_defined(Url, Sessions) of
      true ->  {InterfacePid, CollectorPid}  = gb_trees:get(Url, Sessions),
               exit(InterfacePid, normal),
               exit_collector(CollectorPid),
               laconica_site_model:delete(Url),
               {reply, ok, gb_trees:delete(Url, Sessions)};
      false -> webgnosus_events:warning({session_not_found, Url}), 
               {reply, error, Sessions}
    end.

%%--------------------------------------------------------------------
%% Func: do_start_collectors/1
%% Description: start laconica data collectors
%%--------------------------------------------------------------------
do_collectors_request(Msg, Sessions) ->
    SessionList = gb_trees:to_list(Sessions),
    case length(SessionList) of
      0 -> webgnosus_events:warning(["call open_session."]), error;
      _ -> [send_collector_message(CollectorPid, Msg)|| {_Url, {_InterfacePid, CollectorPid}} <- SessionList]
    end,
    {reply, ok, Sessions}.

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: send_collector_message/2
%% Description: send message to collector
%%--------------------------------------------------------------------
send_collector_message(Pid, Msg) ->
    case Pid of
      -1 -> void;
       _ -> gen_server:cast(Pid, Msg)
    end.

%%--------------------------------------------------------------------
%% Func: spawn_session/2
%% Description: spawn laconica server interface process
%%--------------------------------------------------------------------
spawn_sessions([], Sessions) -> 
    Sessions;

spawn_sessions([Site|Sites], Sessions) ->
    {laconica_sites, Url, PollFrequency} = Site,
    {reply, _Status, UpdatedSessions} = spawn_session(Url, PollFrequency, Sessions),
    spawn_sessions(Sites, UpdatedSessions).

%%--------------------------------------------------------------------
%% Func: spawn_session/3
%% Description: spawn laconica server interface process
%%--------------------------------------------------------------------
spawn_session(Url, PollFrequency, Sessions) ->
    {InterfaceStatus, InterfacePid} = laconica_interface:start_link(Url),
    case PollFrequency of
        0 ->
            {reply, InterfaceStatus, gb_trees:insert(Url, {InterfacePid, -1}, Sessions)};
        _ ->
            {_CollectorStatus, CollectorPid} = laconica_collector:start_link({InterfacePid, PollFrequency}),
            send_collector_message(CollectorPid, collect),
            {reply, InterfaceStatus, gb_trees:insert(Url, {InterfacePid, CollectorPid}, Sessions)}
    end.

%%--------------------------------------------------------------------
%% Func: exit_collector/1
%% Description: exit collector proccess if it exists
%%--------------------------------------------------------------------
exit_collector(Pid) ->
    case Pid of
      -1 -> ok;
       _ -> exit(Pid, normal)
    end.
