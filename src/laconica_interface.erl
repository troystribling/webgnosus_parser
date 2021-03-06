%%%-------------------------------------------------------------------
%%% File        : laconica_interface
%%% Description : fetch data from laconica microbolgging servers and pass to 
%%%               analysis servers
%%%-------------------------------------------------------------------
-module(laconica_interface).

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
init(RootUrl) ->
    process_flag(trap_exit, true),
    webgnosus_events:message({started, {?MODULE, RootUrl, self()}}),
    {ok, RootUrl}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout}     |
%%                                      {noreply, State}                   |
%%                                      {noreply, State, Timeout}          |
%%                                      {stop, Reason, Reply, State}       |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
%% get public timeline
handle_call(_Request, _From, State) ->
    Reply = ok,
   {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(public_timeline, RootUrl) ->
    do_public_timeline(RootUrl),
    {noreply, RootUrl}.

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
start_link(RootUrl) ->
    gen_server:start_link(?MODULE, RootUrl, []).

%%====================================================================
%%% Internal functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: get_public_timeline/1
%% Description: request puiblic time line from specified laconica
%%              server.
%%--------------------------------------------------------------------
do_public_timeline(RootUrl) ->
    Path = RootUrl ++ "/api/statuses/public_timeline.xml",
    case webgnosus_http:get_url(Path) of
        {error} ->
                webgnosus_events:warning({http_get_failed, Path});
            Doc ->
                webgnosus_events:message({public_timeline, RootUrl}),
                try parse_and_save_public_timeline(Doc, RootUrl)
                catch
                  _:X -> 
                      webgnosus_events:warning({xml_parse_failed, [Path, X]})
                end
    end.
        
%%--------------------------------------------------------------------
%% Func: parse_and_save_public_timeline/2
%% Description: parse and save public timeline
%%--------------------------------------------------------------------
parse_and_save_public_timeline(Doc, RootUrl) ->
    ParsedDoc = webgnosus_http:parse_xml(Doc),
    [laconica_status_model:write(S) || S <- laconica_parser:statuses(ParsedDoc, RootUrl)],
    [laconica_user_model:write(U)   || U <- laconica_parser:status_users(ParsedDoc, RootUrl)].
