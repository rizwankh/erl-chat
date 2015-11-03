-module(message_store).

-include_lib("stdlib/include/qlc.hrl").

-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0, save_message/2, find_messages/1, store_message/2, shutdown/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(chat_message,{addressee, body, created_on}).
-record(state, {}).


%% Client API
start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

save_message(Addressee, Body) ->
	gen_server:call(?SERVER, {save_message, Addressee, Body}).

find_messages(Addressee) ->
	case gen_server:call(?SERVER,{find_msgs, Addressee}) of
		{ok, Messages} -> Messages
	end.

shutdown() ->
	gen_server:call(?SERVER, stop).	



%% Helpers
store_message(Addressee, Body) ->
	F = fun() ->
			{_, CreatedOn, _} = erlang:now(),
			mnesia:write(#chat_message{addressee=Addressee, body=Body, created_on=CreatedOn}) end,
	mnesia:transaction(F).

get_message(Addressee) ->
	F = fun() ->
		Query = qlc:q([M#chat_message.body || M <- mnesia:table(chat_message), M#chat_message.addressee =:= Addressee]),
		Results = qlc:e(Query),
		delete_messages(Results),
		Results end,
	{atomic, Messages} = mnesia:transaction(F),
	Messages.

delete_messages(Messages) ->
	F = fun() ->
		lists:foreach(fun(Msg) -> mnesia:delete_object(Msg) end, Messages ) end,
	mnesia:transaction(F).

init_store() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	try
		mnesia:table_info(chat_message, type)
	catch 
		exit: _ ->
			mnesia:create_table(chat_message, [{attributes, record_info(fields, chat_message)},
												{type, bag}, {disc_copies, [node()]}])
	end.


%% gen_server callbacks
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	init_store(),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({save_msg, Addressee, Body}, _From, State) ->
	store_message(Addressee, Body),
	{reply, ok, State};

handle_call({find_msgs, Addressee}, _From, State) ->
	Messages = get_message(Addressee),
	{reply, {ok, Messages}, State};

handle_call(stop, _From, State) ->
	mnesia:stop(),
	{stop, normal, State};


handle_call(_Request, _From, State) ->
  {reply, ignored, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, NewState} |
%% {noreply, NewState, Timeout} |
%% {stop, Reason, NewState}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
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
