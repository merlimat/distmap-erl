%% Author: mat
%% Created: 20/mar/2009
%% Description: TODO: Add description to http_server
-module(http_server).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export( [start_link/0] ).

-behaviour( tcp_server ).
-export( [ new_connection/1, received_data/2, 
		   timeout/1, connection_closed/1 ] ).
%%
%% API Functions
%%

start_link() ->
    tcp_server:start_link( ?MODULE, 8080, [{packet, http}] ).

%%
%% Tcp Server Functions
%%

-record( state, { status = wait_method,
				  method = none,
				  path = none,
				  headers = [],
				  body = none
				} ).

new_connection( From ) ->
	% io:format( "New connection from: ~p~n", [From] ),
	{ok, #state{} }.

received_data( {http_request, Method, {abs_path, Path}, {_,_}}, 
			   State=#state{status=wait_method} ) ->
	NewState = State#state{status=wait_header, method=Method, path=Path},
	{noreply, NewState};
received_data( {http_header, _, Name, _, Value}, 
			   State=#state{status=wait_header} ) ->
	Headers = [ {Name,Value} | State#state.headers ],
	NewState = State#state{status=wait_header, headers=Headers},
	{noreply, NewState };
received_data( http_eoh, State=#state{status=wait_header} ) ->
	% msg complete
	% io:format( "State: ~p~n", [State] ),
	Msg = <<"HTTP/1.1 200 OK\r\n"
		"Content-Type: text/plain\r\n"
	    "Content-Length: 4\r\n"
		"Connection: close\r\n"
		"\r\n"
		"ciao">>,
	{reply_close, Msg, State#state{status=wait_method} }.

timeout( State ) ->
	{noreply, State}.

connection_closed( _State ) ->
	ok. % io:format( "Connection closed.~n" ).
