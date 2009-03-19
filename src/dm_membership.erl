%% Author: mat
%% Created: 05/mar/2009
%% Description: TODO: Add description to membership
-module(dm_membership).

%%
%% Include files
%%

-include( "distmap.hrl" ).

-export( [ add_myself/0, add_node/1, node_is_down/1, remove_node/1, 
		   send_node_list/2 ] ).

% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define( TIMEOUT, 5*1000 ).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], _Opts = []).

add_myself() ->
	gen_server:cast( ?MODULE, add_myself ).

add_node( Node ) ->
	gen_server:cast( ?MODULE, {add_node, Node} ).

remove_node( Node ) ->
	gen_server:cast( ?MODULE, {remove_node, Node} ).

node_is_down( Node ) ->
	gen_server:cast( ?MODULE, {node_is_down, Node} ).

send_node_list( Node, NodeList ) ->
	gen_server:cast( {?MODULE, Node}, {node_list, NodeList} ).
	
%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
	{ok, ring:new() }.

handle_call(_Request, _From, State) ->
    Reply = ok,
	?DEBUG( "Got call...." ),
    {reply, Reply, State, ?TIMEOUT}.

%% ====================================================================

handle_cast( add_myself, Ring ) ->
	ring:add_node( Ring, node(), 100 ),
	{noreply, Ring, ?TIMEOUT};

handle_cast( {add_node, Node}, Ring ) ->
	?DEBUG_( "Add node: ~p", [Node] ),
	ring:add_node( Ring, Node, 100 ),
	do_monitor( Ring ),
	
	case i_am_on_preference_list( Ring, Node ) of 
		false -> ok;
		true -> send_node_list( Node, ring:physical_nodes(Ring) )
	end,
	{noreply, Ring, ?TIMEOUT};

handle_cast( {remove_node, Node}, Ring ) ->
	?DEBUG_( "remove node: ~p", [Node] ),
	catch ring:remove_node( Ring, Node ),
	do_monitor( Ring ),
	{noreply, Ring, ?TIMEOUT};

handle_cast( {node_is_down, Node}, Ring ) ->
	?INFO_( "Node is down: ~p", [Node] ),
	ring:remove_node( Ring, Node ),
	?DEBUG_( "Nodes: ~p", [ring:physical_nodes(Ring)] ),
	dm_finder:announce_node_is_down( Node ),
	
	do_monitor( Ring ),
	{noreply, Ring, ?TIMEOUT};

handle_cast( {node_list, []}, Ring ) ->
	%% ?DEBUG_( "Received Node list: ~p", [ring:physical_nodes(Ring)] ),
	do_monitor( Ring ),
	{noreply, Ring, ?TIMEOUT};
handle_cast( {node_list, [FirstNode|Rest]}, Ring ) ->
	ring:add_node( Ring, FirstNode, 100 ),
	handle_cast( {node_list, Rest}, Ring ).

%% ====================================================================

handle_info(timeout, Ring) ->
	% Send the node list to a random node
	Node = get_random_node(Ring),
	send_node_list( Node, ring:physical_nodes(Ring) ),
    {noreply, Ring, ?TIMEOUT};

handle_info(Info, State) ->
	?DEBUG_( "Got Info: ~p", [Info] ),
    {noreply, State, ?TIMEOUT}.

terminate(Reason, _State) ->
	?DEBUG_( "Stopping membership: ~p", [Reason] ).

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions 
%% ====================================================================

do_monitor( Ring ) -> 
	NextNode = ring:get_next_physical_node( Ring, node() ),
	if  NextNode == node() -> dm_monitor:end_monitor();
		true               -> dm_monitor:start_monitor( NextNode )
	end.

i_am_on_preference_list( Ring, Object ) ->
	PrefList = ring:get_preference_list( Ring, Object, 3 ),
	util:contains( node(), PrefList ).
	
get_random_node( Ring ) ->
	List = ring:physical_nodes( Ring ),
	I = random:uniform( length(List) ),
	Node = lists:nth( I, List ),
	if Node == node() ->
		get_random_node( Ring );
		true -> Node
	end.
