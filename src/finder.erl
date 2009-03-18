%%% -------------------------------------------------------------------
%%% Author  : mat
%%% Description :
%%%
%%% Created : 05/mar/2009
%%% -------------------------------------------------------------------
-module(finder).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { sendsock, recvsock, addr, port}).

%% ====================================================================
%% External functions
%% ====================================================================

start() ->
  gen_server:start_link ({ local, ?MODULE }, ?MODULE, [ { 226, 0, 0, 1 }, 20000 ], []).


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Addr, Port]) ->
	io:format( "Broadcast addr: ~s~n", [addr(Addr, Port)]),
	process_flag (trap_exit, true),
	
  	Opts = [ { active, true },
    	     { ip, Addr },
        	 { add_membership, { Addr, { 0, 0, 0, 0 } } },
			 { multicast_loop, true },
             { reuseaddr, true },
           list ],

	{ ok, RecvSocket } = gen_udp:open (Port, Opts),

	{ ok, announce(#state{ recvsock = RecvSocket,
                         sendsock = send_socket(),
                         addr = Addr,
                         port = Port }) }.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, From, State) ->
	io:format( "Handle call from ~p~n", From ),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(Msg, State) ->
	io:format( "Handle cast: ~p~n", Msg ),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info ({ udp, Socket, IP, Port, Packet },
             State=#state{ recvsock = Socket }) ->
	{ noreply, process_packet( Packet, IP, Port, State ) };
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	gen_udp:close (State#state.recvsock),
	gen_udp:close (State#state.sendsock),
	io:format( "Node finder stopped. reason:~p ~n", [Reason] ),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

announce( State ) ->
  NodeString = atom_to_list( node() ),
  % Time = seconds(),
  % Mac = mac ([ <<Time:64>>, NodeString ]),
  Msg = "ANNOUNCE " ++ NodeString, % [ "DISCOVERV2 ", Mac, " ", <<Time:64>>, " ", NodeString ],
  ok = gen_udp:send( State#state.sendsock,
                     State#state.addr,
                     State#state.port,
                     Msg ),
  State.

process_packet( "ANNOUNCE " ++ Rest, IP, InPortNo, State ) ->
	NodeName = list_to_atom(Rest),
	if 
		NodeName =:= node() -> 
			% Ignore auto-announce
			ok;
		
		true ->
			io:format( "Announced ~p (~s)~n", [NodeName, addr(IP, InPortNo)] ),
			membership ! {add, NodeName}
	end,
  % Falling a mac is not really worth logging, since having multiple
  % cookies on the network is one way to prevent crosstalk.  However
  % the packet should always have the right structure.

%%   try
%%     <<Mac:20/binary, " ", 
%%       Time:64, " ",
%%       NodeString/binary>> = list_to_binary (Rest),
%% 	
%%     case { mac ([ <<Time:64>>, NodeString ]), abs (seconds () - Time) } of
%%       { Mac, AbsDelta } when AbsDelta < 300 ->
%%         net_adm:ping (list_to_atom (binary_to_list (NodeString)));
%%       { Mac, AbsDelta } ->
%%         error_logger:warning_msg ("expired DISCOVERV2 (~p) from ~p:~p~n",
%%                                   [ AbsDelta,
%%                                     IP,
%%                                     InPortNo ]);
%%       _ ->
%%         ok
%%     end
%%   catch
%%     error : { badmatch, _ } ->
%%       error_logger:warning_msg ("bad DISCOVERV2 from ~p:~p~n", 
%%                                 [ list_to_binary (Rest),
%%                                   IP,
%%                                   InPortNo ])
%%   end,
  State;
process_packet (_Packet, _IP, _InPortNo, State) ->
	io:format( "Unknown data received~n" ),
 	State.

send_socket() ->
  SendOpts = [ { ip, { 0, 0, 0, 0 } },
               { multicast_loop, true } ],
  { ok, SendSocket } = gen_udp:open (0, SendOpts),
  SendSocket.

addr( {A,B,C,D}, Port ) -> 
	lists:flatten( io_lib:format( "~p.~p.~p.~p:~p", [A,B,C,D,Port]) ).

