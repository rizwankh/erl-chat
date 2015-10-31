-module (mucc).

-compile(export_all).

-define(SERVER, mucc).



start() ->
	server_util:start(?SERVER, {mucc, server_loop, [dict:new()]}).

stop() ->
	server_util:stop(?SERVER).

register_nick(Nick) ->
	global:send(?SERVER, {register, Nick, self()}),
	receive
		ok -> ok;
		{error, Error} -> Error
	end.

poll(Nick) ->
	global:send(?SERVER, {poll, Nick, self()}),
	receive
		{ok, Messages} -> Messages;
		{error, Error} -> Error
	end.

send_message(Sender, Addressee, MessageBody) ->
	global:send(?SERVER, {send_chat_msg, Sender, Addressee, MessageBody}).
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


server_loop(Proxies) ->
	receive
		{register, Nick, Caller} ->
			case dict:find(Nick, Proxies) of
				error ->
					Pid = spawn(fun() -> proxy_client([]) end),
					message_router:register(Nick, Pid),
					Caller ! ok,
					server_loop(dict:store(Nick, Pid, Proxies));
				{ok, _} ->
					Caller ! {error, duplicate_nick_found},
					server_loop(Proxies)
			end;
		{poll, Nick, Caller} ->
			case dict:find(Nick, Proxies) of
				error ->
					Caller ! {error, unknown_nick};
				{ok, ProxyPid} ->
					ProxyPid ! {get_messages, self()},
					receive
						{messages, Messages} ->
							Caller ! {ok, Messages}
					end,
					server_loop(Proxies)
			end;
		{send_chat_msg, Sender, Addressee, MessageBody} ->
			case dict:find(Sender, Proxies) of
				error ->
					ok;
				{ok, ProxyPid} ->
					ProxyPid ! {send_chat_msg, Addressee, MessageBody}
			end,
			server_loop(Proxies);
		Unknown ->
			io:format("Unknow msg format: ~p~n", [Unknown]),
			server_loop(Proxies)
	end.
