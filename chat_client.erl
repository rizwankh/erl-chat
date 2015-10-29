-module (chat_client).

-compile(export_all).

%testing git revert

start_router() ->
	message_router:start().

stop_router() ->
	message_router:stop().

register(Nick) ->
	Pid = spawn(chat_client, handle_messages, [Nick]),
	message_router:register(Nick, Pid).

unregister(Nick) ->
	message_router:unregister(Nick). 

send_message(Nick, MessageBody) ->
	message_router:send_message(Nick, MessageBody).

handle_messages(Nick) ->
	receive
		{printmsg, MessageBody} ->
			io:format("~p received: ~p~n", [Nick, MessageBody]),
			handle_messages(Nick);
		stop ->
			ok
	end.
