%% Author: mat
%% Created: 06/mar/2009
%% Description: TODO: Add description to ring
-module(ring).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-compile(export_all).

%%
%% API Functions
%%

new() -> 
	ets:new(nodes,[ordered_set]).

add_node( Ring, Node, N ) ->
	ets:insert( Ring, {gen_key(Node), Node, real, N} ),
	lists:foreach( fun(X) -> 
				       Key = gen_key(Node, X),
					   ets:insert( Ring, {Key,Node, virtual, N} )
			       end, lists:seq(1, N-1) ), 
	ok.

remove_node( Ring, Node ) ->
	BaseKey = gen_key(Node),
	[{_, Node, _, N}] = ets:lookup( Ring, BaseKey ),
	ets:delete( Ring, BaseKey ),
	lists:foreach( fun(X) -> 
					   ets:delete( Ring, gen_key(Node, X) )
			       end, lists:seq(1, N-1) ), 
	ok.

get_node( Ring, Object ) ->
	Key = gen_key( Object ), 
	NodeKey = case ets:next( Ring, Key ) of
				  '$end_of_table' -> ets:first( Ring );
				  K -> K
			  end,
	[{NodeKey, Node, _, _}] = ets:lookup( Ring, NodeKey ),
	Node.

%%
%% Local Functions
%%

gen_key( Arg ) ->
	erlang:md5( term_to_binary(Arg) ).

gen_key( Node, N ) when is_integer(N) ->
	erlang:md5( term_to_binary({Node, N}) ).

get_node_replicas( Node, [{First,N}|Rest] ) ->
	if Node =:= First ->
		   N;
	   true ->
		   get_node_replicas(Node, Rest)
	end.

test() ->
	R = ring:new(),
	NodeList = [a, b, c, d, e, f, g, h, i, l, m, n, o, p, q, r, s, t, u, v, z],
	lists:foreach(fun(Node) -> ring:add_node(R, Node, 100) end, 
						NodeList ),
	
	N = 1000000,
 	{Time, Res} = timer:tc( ?MODULE, get_node_test, [R, N, NodeList] ),
 	io:format( "Res: ~p~n", [Res] ),
 	io:format( "Time:     ~p ms~n", [Time / 1000] ),
 	io:format( "Avg Time: ~p us~n", [Time / N ] ).

get_node_test( R, N, NodeList ) ->
	T = ets:new(name, [ordered_set]),
	lists:foreach( fun(X) -> ets:insert(T, {X,0} ) end, NodeList ),
	get_node_test1( R, T, N ),
	ets:tab2list( T ).

get_node_test1( _R, _T, 0 ) -> ok;
get_node_test1( R, T, N ) ->
	Node = ring:get_node( R, N ),
	ets:update_counter(T, Node, 1),
	get_node_test1(R, T, N-1).

