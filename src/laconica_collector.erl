%%%-------------------------------------------------------------------
%%% File        : laconica_collector
%%% Description : polls specified laconica site a specifoed frequency
%%%-------------------------------------------------------------------
-module(laconica_collector).

-behaviour(gen_server).

%% API
-export([
         start_link/1
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
init({Pid, PollFrequency}) ->
    process_flag(trap_exit, true),
    webgnosus_events:message({started, {?MODULE, {Pid, PollFrequency}, self()}}),
    {ok, {Pid, PollFrequency, true}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout}     |
%%                                      {noreply, State}                   |
%%                                      {noreply, State, Timeout}          |
%%                                      {stop, Reason, Reply, State}       |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% collect data from laconica server
handle_call(collect, _From, {Pid, PollFrequency, Collect}) ->
    if 
      Collect =:= true -> 
          do_collection(Pid),
          timer:sleep(PollFrequency * 1000),
          gen_server:call(self(), collect)
    end,
    {reply, ok, {Pid, PollFrequency, Collect}};

%% start collecting data from laconica server
handle_call(start_collection, _From, {Pid, PollFrequency, _Collect}) ->  
    {reply, ok, {Pid, PollFrequency, true}};

%% stop collecting data from laconica server
handle_call(stop_collection, _From, {Pid, PollFrequency, _Collect}) ->  
    {reply, ok, {Pid, PollFrequency, false}}.

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
terminate(Reason, State) ->
    webgnosus_events:message({stopped, {?MODULE, self(), Reason, State}}),
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
start_link({Pid, PollFrequency}) ->
    gen_server:start_link(?MODULE, {Pid, PollFrequency}, []).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: do_collection/1
%% Description: collect data from laconica server
%%--------------------------------------------------------------------
do_collection(Pid) ->
    gen_server:call(Pid, public_timeline).
        
    
    