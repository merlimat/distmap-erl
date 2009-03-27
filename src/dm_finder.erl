%%% -------------------------------------------------------------------
%%% Author  : mat
%%% Description :
%%%
%%% Created : 05/mar/2009
%%% -------------------------------------------------------------------
-module(dm_finder).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include( "distmap.hrl" ).

%% --------------------------------------------------------------------
%% External exports
-export([ start_link/0, announce_myself/1, announce_node_is_down/1, 
		  get_external_ip/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { sendsock, recvsock, addr, port}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
	gen_server:start_link ({ local, ?MODULE }, ?MODULE, [], []).

announce_myself( Type ) ->
	gen_server:cast( ?MODULE, {announce_myself, Type}).

announce_node_is_down( Node ) ->
	gen_server:cast( ?MODULE, {announce_node_is_down, Node}).

get_external_ip() ->
	% First assing a temp name to the node
	% to avoid warnings on other nodes
	{A,B,C} = now(),
	Name = ?L2A( ?I2L(A+B+C) ++ "@127.0.0.1" ),
	net_kernel:start( [Name, longnames] ),
	Self = self(),
 	gen_server:cast( ?MODULE, {discover_ip, Self}),
 	IP = receive
 		{ip, Self, Addr} -> Addr
 	end,
	net_kernel:stop(),
	IP.

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
init( [] ) ->
	{Addr, Port} = dm_config:get_addr( multicast_channel ),
	?DEBUG_( "Broadcast addr: ~s", [util:format_addr(Addr, Port)]),
	
  	Opts = [ { active, true },
    	     { ip, Addr },
        	 { add_membership, { Addr, { 0, 0, 0, 0 } } },
			 { multicast_loop, true },
             { reuseaddr, true },
           list ],

	{ ok, RecvSocket } = gen_udp:open( Port, Opts ),
	
	SendOpts = [ { ip, { 0, 0, 0, 0 } },
                 { multicast_loop, true } ],
	{ ok, SendSocket } = gen_udp:open( 0, SendOpts ),

	{ ok, #state{ recvsock = RecvSocket,
                  sendsock = SendSocket,
                  addr = Addr,
                  port = Port } }.

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
	?DEBUG_( "Handle call from ~p", From ),
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( {announce_myself, Type}, State) ->
	Msg = "ANNOUNCE " ++ ?A2L(Type) ++ " " ++ ?A2L( node() ),
	send_msg( Msg, State ),
    {noreply, State };

handle_cast( {announce_node_is_down, Node}, State) ->
	send_msg( "NODE IS DOWN " ++ ?A2L(Node), State ),
    {noreply, State };

handle_cast( {discover_ip, Proc}, State) ->
	Pid = binary_to_list( term_to_binary( Proc ) ),
	send_msg( "DISCOVER IP " ++ Pid, State ),
	{noreply, State };

handle_cast(Msg, State) ->
	?DEBUG_( "Handle cast: ~p", Msg ),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info( {udp, Socket, IP, _Port, Packet},
              State=#state{ recvsock = Socket }) ->
	process_packet( Packet, IP ),
	{ noreply, State }.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, State) ->
	gen_udp:close (State#state.recvsock),
	gen_udp:close (State#state.sendsock),
	?DEBUG_( "Node finder stopped. reason:~p", Reason ),
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

process_packet( "ANNOUNCE " ++ Rest, _IP ) ->
	[TypeName, NodeName] = string:tokens( Rest, " " ),
	_Type = ?L2A(TypeName),
	Node = ?L2A(NodeName),
	?DEBUG_( "Process packet node: ~p", [node()] ),
	if 
		Node =:= node() -> 
			% Ignore auto-announce
			ok;
		
		true ->
			?INFO_( "Announced ~p",	[Node] ),
			dm_membership:add_node( Node )
	end;

process_packet( "NODE IS DOWN " ++ NodeName, _IP ) ->
	?WARNING_( "Node is down: '~s'", NodeName ),
	dm_membership:remove_node( ?L2A(NodeName) );

process_packet( "DISCOVER IP " ++ Proc, IP ) ->
	Pid = ?B2T( ?L2B(Proc) ),
	Pid ! {ip, Pid, IP}.

send_msg( Msg, State ) ->
	ok = gen_udp:send( State#state.sendsock,
                       State#state.addr,
                       State#state.port,
                       Msg ).
