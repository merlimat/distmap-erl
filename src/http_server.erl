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
          version = none,
          keep_alive = none,
          path = none,
                  headers = [],
          body = none
    } ).

new_connection( From ) ->
    % io:format( "New connection from: ~p~n", [From] ),
    {ok, #state{} }.

received_data( {http_request, Method, {abs_path, Path}, Version}, 
               State=#state{status=wait_method} ) ->
    NewState = State#state{status=wait_header, method=Method, 
            path=Path, version=Version},
    {noreply, NewState};
received_data( {http_header, _, Name, _, Value},
               State=#state{status=wait_header}  ) ->
    Headers = [ {Name,Value} | State#state.headers ],
    State1 = case Name of
        "Connection" -> 
            KeepAlive = if Value == "keep-alive" -> true;
                    true -> false 
                    end, 
            State#state{ keep_alive=KeepAlive };
        _ -> State
    end,
    NewState = State1#state{status=wait_header, headers=Headers},
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
    Action = case is_keep_alive( State#state.keep_alive, State#state.version ) of 
            true -> reply;
            false -> reply_close
        end,
    {Action, Msg, #state{status=wait_method} }.

timeout( State ) ->
    {noreply, State}.

connection_closed( _State ) ->
    ok. % io:format( "Connection closed.~n" ).


%%% Local functions
is_keep_alive( true, _Version ) -> true; 
is_keep_alive( false, _Version ) -> false; 
is_keep_alive( _, {1,0} ) -> false;
is_keep_alive( _, {1,1} ) -> true.

