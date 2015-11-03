-module(mucc).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, stop/0, register_nick/1, poll/1, send_message/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Client API
start_link() ->
	gen_server:start_link({global, ?SERVER}, ?SERVER, [], []).

stop() ->
	gen_server:cast({global, ?SERVER}, stop).

register_nick(Nick) ->
	gen_server:call({global, ?SERVER}, {register, Nick}).

poll(Nick) ->
	case gen_server:call({global, ?SERVER}, {poll, Nick}) of
		{ok, Messages} -> Messages;
		{error, Error} -> Error
	end.

send_message(Sender, Addressee, MessageBody) ->
	gen_server:cast({global, ?SERVER}, {send_chat_msg, Sender, Addressee, MessageBody}).

%% gen_server callbacks
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
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
handle_call({register, Nick}, _From, Proxies) ->
			case dict:find(Nick, Proxies) of
				error ->
					Pid = spawn(fun() -> proxy_client([]) end),
					message_router:register(Nick, Pid),
					NewProxies = dict:store(Nick, Pid, Proxies),
					{reply, ok, NewProxies};
				{ok, _} ->
					{reply, {error, duplicate_nick_found}, Proxies}
			end;

handle_call({poll, Nick}, _From, Proxies) ->
			case dict:find(Nick, Proxies) of
				error ->
					{reply, {error, unknown_nick}, Proxies};
				{ok, ProxyPid} ->
					ProxyPid ! {get_messages, self()},
					receive
						{messages, Messages} ->
							{reply, {ok, Messages}, Proxies}
					end
			end;

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, NewState} |
%% {noreply, NewState, Timeout} |
%% {stop, Reason, NewState}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({send_chat_msg, Sender, Addressee, MessageBody}, _From, Proxies) ->
			case dict:find(Sender, Proxies) of
				error ->
					ok;
				{ok, ProxyPid} ->
					ProxyPid ! {send_chat_msg, Addressee, MessageBody}
			end,
			{noreply, Proxies};

handle_cast(stop, State) ->
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
proxy_client(Messages) ->
	receive
		{printmsg, Body} ->
			proxy_client([Body | Messages]);
		{get_messages, Caller } ->
			Caller ! {messages, lists:reverse(Messages)},
			proxy_client([]);
		{send_chat_msg, Addressee, MessageBody} ->
			message_router:send_message(Addressee, MessageBody),
			proxy_client([]);
		stop ->
			io:format("Proxy stopping...~n"),
			ok 
	end.