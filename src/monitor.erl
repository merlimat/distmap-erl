%% Author: mat
%% Created: 05/mar/2009
%% Description: TODO: Add description to monitor
-module(monitor).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([start/0, stop/0, loop_start/0, loop/1, start_monitor/1, end_monitor/0]).

%%
%% API Functions
%%

start() -> register( monitor, spawn(?MODULE, loop_start, [])).
stop() -> monitor ! stop.

start_monitor( Node ) -> 
	monitor ! {monitor_node, Node}.

end_monitor() ->
	monitor ! {monitor_none}.

%%
%% Local Functions
%%
loop_start() -> 
	receive
		{monitor_node, Node} ->
			loop( Node );
		
		{stop} -> ok
	end.

loop( Node ) ->
	io:format( "Monitoring: ~p~n", [Node] ),
	flush_messages(),
	monitor_node( Node, true ),
	receive
		{nodedown, NodeDown} ->
			io:format( "Node ~p is down!!!~n", [NodeDown] ),
			monitor_node( NodeDown, false ),
			membership ! {dead, NodeDown},
			loop_start();
		
		{monitor_node, NewNode} ->
			monitor_node( Node, false ),
			loop( NewNode );
		
		{monitor_none} ->
			monitor_node( Node, false ),
			loop_start();
		
		{stop} -> ok;
		
		Any ->
			io:format( "Unexpected message: ~p~n", Any )
	end.

flush_messages() ->
	receive
		_Any -> ok
	after 0 ->
		ok
	end.
