%%
-module(tcp_server).
-author('Jesse E.I. Farmer <jesse@20bits.com>').
-author('Matteo Merli <mm@merlimat.org>').
-behavior(gen_server).

-export([init/1, code_change/3, handle_call/3, 
	  	 handle_cast/2, handle_info/2, terminate/2]).
-export([accept_loop/3]).
-export([start_link/2, start_link/3]).
-export([behaviour_info/1]).

%% Define the tcp_server behaviour
behaviour_info( callbacks ) ->
	[ {new_connection, 1},
	  {received_data, 2},
	  {timeout, 1},
	  {connection_closed,1}
	];
behaviour_info(_) -> undefined.

-record(server_state, { module,
        				lsocket = null } ).

start_link( Module, Port ) ->
    start_link( Module, Port, [] ).

start_link( Module, Port, Options ) ->
    gen_server:start_link( {local, Module}, ?MODULE, {Module, Port, Options}, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define( DefaultOptions, [ binary, 
						   {packet, 0}, 
					       {active, false},
				   	       {reuseaddr, true} ] ).

init( {Module, Port, Options} ) ->
	TcpOptions = lists:merge( ?DefaultOptions, Options ),
    case gen_tcp:listen( Port, TcpOptions ) of
        {ok, LSocket} ->
            State = #server_state{ lsocket=LSocket, module=Module },
            {ok, accept( State )};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_cast({accepted, _}, State=#server_state{}) ->
    {noreply, accept(State)}.

% To be more robust we should be using spawn_link and trapping exits
accept(State = #server_state{lsocket=LSocket, 
							 module=Module} ) ->
    proc_lib:spawn_link( ?MODULE, accept_loop, 
			  			 [self(), LSocket, Module]),
    State.

% These are just here to suppress warnings.
handle_call(_Msg, _Caller, State) -> {noreply, State}.
handle_info(_Msg, Library) -> {noreply, Library}.
terminate(_Reason, _Library) -> ok.
code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


accept_loop( Server, LSocket, Module ) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    % Let the server spawn a new process and replace this loop
    % with the echo loop, to avoid blocking 
    gen_server:cast( Server, {accepted, self()} ),
	{ok, From} = inet:peername( Socket ),
	{ok, ConnState} = Module:new_connection( From ),
	connection_loop( Socket, Module, ConnState, infinity ).

connection_loop( Socket, Module, ConnState, Timeout ) ->
	case catch gen_tcp:recv( Socket, 0, Timeout ) of
        {ok, Data} ->
			Result = Module:received_data( Data, ConnState ), 
			process_result( Socket, Module, Result );
		
		{error, timeout} ->
			Result = Module:timeout( ConnState ),
			process_result( Socket, Module, Result );

		{error, closed} ->
			Module:connection_closed( ConnState );
		
		Any -> 
			io:format( "Error: ~p~n", [Any] )
    end.

process_result( Socket, Module, CallbackResult ) ->
	case CallbackResult of
		{reply, ReplyData, NewState} ->
			gen_tcp:send( Socket, ReplyData ), 
			connection_loop( Socket, Module, NewState, infinity );
		
		{reply, ReplyData, NewState, Timeout} ->
			gen_tcp:send( Socket, ReplyData ), 
			connection_loop( Socket, Module, NewState, Timeout );
				
		{reply_close, ReplyData, NewState} ->
			gen_tcp:send( Socket, ReplyData ),
			gen_tcp:close( Socket ), 
			Module:connection_closed( NewState );
		
		{noreply, NewState} ->
			connection_loop( Socket, Module, NewState, infinity );
		
		{noreply, NewState, Timeout} ->
			connection_loop( Socket, Module, NewState, Timeout );
				
		{close, NewState} ->
			gen_tcp:close( Socket ), 
			Module:connection_closed( NewState );

		{'EXIT', Reason} ->
			io:format( "Exit: ~p~n", [Reason] ),
			exit( Reason );
	
		Any -> 
			io:format( "unexpected result: ~p~n", [Any] )
	end.
	
