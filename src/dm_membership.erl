%% Author: mat
%% Created: 05/mar/2009
%% Description: TODO: Add description to membership
-module(dm_membership).

%%
%% Include files
%%

-include( "distmap.hrl" ).

%%
%% Exported Functions
%%
%% -export([start_link/0, stop/0, loop/1]).

%%
%% API Functions
%%

%% start_link() -> 
%% 	finder:start(),
%% 	monitor:start(),
%% 	register( membership, spawn(?MODULE, loop, [[]])).
%% 
%% stop() -> 
%% 	membership ! stop.

%%
%% Local Functions
%%
%% loop( NodeList ) -> 
%% 	io:format( "NodeList: ~p~n", [NodeList] ),
%% 	receive
%% 		{add, Node} -> 
%% 			io:format( "Added node: ~p~n", [Node]),
%% 			monitor:start_monitor( Node ),
%% 			loop( [Node|NodeList] );
%% 		
%% 		{dead, Node} ->
%% 			loop( lists:delete(Node, NodeList) );
%% 		
%% 		stop -> 
%% 			io:format( "Stopping membership process.~n" );
%% 	
%% 		Any ->
%% 			io:format( "Unexpected message: ~p~n", Any )
%% 	end.

% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], _Opts = []).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
	{ok, ring:new()}.

handle_call(_Request, _From, State) ->
    Reply = ok,
	?DEBUG( "Got call...." ),
    {reply, Reply, State}.

handle_cast( _Msg, State ) ->
	?DEBUG( "Got cast" ),
    {noreply, State}.

handle_info(_Info, State) ->
	?DEBUG( "Got Info...." ),
    {noreply, State}.

terminate(_Reason, _State) ->
	?DEBUG( "Stopping membership~n" ).

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
