-module(server_util).

-compile(export_all).


start(ServerName, {M, F, A}) ->
	global:trans({ServerName, ServerName},
		fun() ->
			case global:whereis_name(ServerName) of
				undefined ->
					Pid = spawn(M, F, A),
					global:register_name(ServerName, Pid);
				_ -> ok
			end
		end). 

stop(ServerName) ->
	global:trans({ServerName, ServerName},
		fun() ->
			case global:whereis_name(ServerName) of
				undefined -> ok;
				_ -> global:send(ServerName, shutdown)
			end
		end).