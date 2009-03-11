%% Author: mat
%% Created: 05/mar/2009
%% Description: TODO: Add description to membership
-module(membership).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, stop/0, loop/1]).

%%
%% API Functions
%%

start() -> 
	finder:start(),
	monitor:start(),
	register( membership, spawn(?MODULE, loop, [[]])).

stop() -> 
	membership ! stop.

%%
%% Local Functions
%%
loop( NodeList ) -> 
	io:format( "NodeList: ~p~n", [NodeList] ),
	receive
		{add, Node} -> 
			io:format( "Added node: ~p~n", [Node]),
			monitor:start_monitor( Node ),
			loop( [Node|NodeList] );
		
		{dead, Node} ->
			loop( lists:delete(Node, NodeList) );
		
		stop -> 
			io:format( "Stopping membership process.~n" );
	
		Any ->
			io:format( "Unexpected message: ~p~n", Any )
	end.
