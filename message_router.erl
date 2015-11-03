-module(message_router).

-behaviour(gen_server).

-define(SERVER, ?MODULE).


%% API
-export([start_link/0, stop/0, send_message/2, register/2, unregister/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Client API
start_link() ->
	gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

stop() ->
	gen_server:handle_cast({global, ?SERVER}, stop).

send_message(Nick, MessageBody) ->
	gen_server:call({global, ?SERVER}, {send_chat_msg, Nick, MessageBody}).

register(Nick, ClientPid) ->
	gen_server:call({global, ?SERVER}, {register_nick, Nick, ClientPid}).

unregister(Nick) ->
	gen_server:call({global, ?SERVER} ,{unregister_nick, Nick}).


%% gen_server callbacks
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	message_store:start_link(),
	{ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({send_chat_msg, Nick, MessageBody}, _From, Clients) ->
	case dict:find(Nick, Clients) of 
				{ok, ClientPid} -> ClientPid ! {printmsg, MessageBody};
				error ->
					message_store:store_message(Nick, MessageBody), 
					io:format("Message Archived for : ~p~n",[Nick])
	end,
	{reply, ok, Clients};

handle_call({register_nick, Nick, ClientPid}, _From, Clients) ->
			Messages = message_store:find_messages(Nick),
			lists:foreach(fun(Msg) -> ClientPid ! {printmsg, Msg} end, Messages),
			NewClients = dict:store(Nick, ClientPid, Clients),
			{reply, ok, NewClients};

handle_call({unregister_nick, Nick}, _From, Clients) ->
			NewClients = case dict:find(Nick, Clients) of
				{ok, ClientPid} -> 
					ClientPid ! stop,
					dict:erase(Nick, Clients);
				error -> 
					io:format("Unknown Client: ~p", [Nick]),
					Clients
			end,
			{reply, ok, NewClients};

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, NewState} |
%% {noreply, NewState, Timeout} |
%% {stop, Reason, NewState}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
	message_store:shutdown(),
	{stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling all non-call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is
%% about to terminate. It should be the opposite of Module:init/1 and
%% do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState} %%
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private functions
