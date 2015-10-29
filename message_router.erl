-module (message_router).

-compile(export_all).

-define(SERVER, message_router).


start() ->
	global:trans({?SERVER, ?SERVER},
		fun() ->
			case global:whereis_name(?SERVER) of
				undefined ->
					Pid = spawn(message_router, route_messages, [dict:new()]),
					global:register_name(?SERVER, Pid);
				_ -> ok
			end
		end). 	
	

stop() ->
	global:trans({?SERVER, ?SERVER}, 
		fun() ->
			case global:whereis_name(?SERVER) of
				undefined -> ok;
				_ -> global:send(?SERVER, shutdown)
			end
		end).
	

send_message(Nick, MessageBody) ->
	global:send(?SERVER, {send_chat_msg, Nick, MessageBody}).

register(Nick, ClientPid) ->
	global:send(?SERVER, {register_nick, Nick, ClientPid}).

unregister(Nick) ->
	global:send(?SERVER ,{unregister_nick, Nick}).

route_messages(Clients) ->
	receive
		{send_chat_msg, Nick, MessageBody} ->
			case dict:find(Nick, Clients) of 
				{ok, ClientPid} -> ClientPid ! {printmsg, MessageBody};
				error -> io:format("Unknown Client: ~p~n",[Nick])
			end,
			route_messages(Clients);
		{register_nick, Nick, ClientPid} ->
			NewClients = dict:store(Nick, ClientPid, Clients),
			route_messages(NewClients);
		{unregister_nick, Nick} ->
			case dict:find(Nick, Clients) of
				{ok, ClientPid} -> ClientPid ! stop;
				error -> io:format("Unknown Client: ~p", [Nick])	
			end,
			route_messages(dict:erase(Nick, Clients));
		shutdown ->
			io:format("Shutting down ~n");
		Oops ->
			io:format("Unknown message! ~p~n",[Oops]),
			route_messages(Clients)
	end.

