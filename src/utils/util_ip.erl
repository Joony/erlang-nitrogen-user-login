-module(util_ip).
-compile(export_all).

ip() ->
    %% Parts adapted from mochiweb_request.erl
    Socket = wf_platform:get_socket(),
        case inet:peername(Socket) of
	    {ok, {Addr = {X, _, _, _}, _Port}} when X == 10;
	     Addr == {127,0,0,1} ->
		    case wf_platform:get_header(x_forwarded_for) of
			undefined -> Addr;
			Hosts -> case inet_parse:address(string:strip(lists:last(string:tokens(Hosts, ",")))) of
				          {ok, AddrFwd} -> AddrFwd;
				          _ -> Addr
						    end
				         end;
	    {ok, {Addr, _Port}} -> Addr
    end.
