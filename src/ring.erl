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
-export( [new/0, add_node/3, remove_node/2, get_node/2, get_preference_list/3, 
		  get_next_physical_node/2, test/0, get_node_test/3] ).

-import( util, [for/3] ).

%%
%% API Functions
%%

new() -> 
	ets:new(nodes,[ordered_set]).

add_node( Ring, Node, N ) ->
	ets:insert( Ring, {gen_key(Node), Node, real, N} ),
	for( 1, N-1, fun(X) -> 
				     Key = gen_key(Node, X),
					 ets:insert( Ring, {Key,Node, virtual, N} )
			     end ), 
	ok.

remove_node( Ring, Node ) ->
	BaseKey = gen_key(Node),
	[{_, Node, _, N}] = ets:lookup( Ring, BaseKey ),
	% Delete real node
	ets:delete( Ring, BaseKey ),
	
	% Delete virtual nodes
	for( 1, N-1, fun(X) -> 
				    ets:delete( Ring, gen_key(Node, X) )
			     end ),
	ok.

get_node( Ring, Object ) ->
	Key = gen_key( Object ),
	{_, Node, _} = get_next_node( Ring, Key ),
	Node.

%%

get_preference_list( Ring, Object, N ) ->
	Key = gen_key( Object ),
	List = get_preference_list0( Ring, first, none, Key, N, [] ),
	lists:reverse( List ).

get_preference_list0( _Ring, _, _BaseKey, _Key, 0, Acc ) -> Acc;
get_preference_list0( _Ring, not_first, BaseKey, Key, _N, Acc ) when BaseKey == Key 
  	-> Acc;
get_preference_list0( Ring, Type, BaseKey, Key, N, Acc ) ->
	{NextKey, Node, _} = get_next_node( Ring, Key ),
	{BaseKey1, NextType} = if (Type == first) and (BaseKey == none) 
								   -> {NextKey, first}; 
			      			  true -> {BaseKey, not_first}
			               end,
	IsMember = lists:member( Node, Acc ) =:= true,
	if IsMember ->
		 get_preference_list0( Ring, NextType, BaseKey1, NextKey, N, Acc );
	   true ->
		 get_preference_list0( Ring, NextType, BaseKey1, NextKey, N-1, [Node|Acc] )
	end.

%%

get_next_physical_node( Ring, Node ) ->
	Key = gen_key( Node ),
	get_next_physical_node0( Ring, Key ).

get_next_physical_node0( Ring, Key ) ->
	{NextKey, Node, Type} = get_next_node( Ring, Key ),
	case Type of
		real    -> Node;
		virtual -> get_next_physical_node0( Ring, NextKey )
	end.

%%
%% Local Functions
%%

gen_key( Arg ) ->
	erlang:md5( term_to_binary(Arg) ).

gen_key( Node, N ) when is_integer(N) ->
	erlang:md5( term_to_binary({Node, N}) ).

get_next_node( Ring, Key ) ->
	NodeKey = case ets:next( Ring, Key ) of
				  '$end_of_table' -> ets:first( Ring );
				  K -> K
			  end,
	[{NodeKey, Node, Type, _}] = ets:lookup( Ring, NodeKey ),
	{NodeKey, Node, Type}.

%% 
%% Test Functions
%%

test() ->
	R = ring:new(),
	NodeList = [a, b, c, d, e, f, g, h, i, l, m, n, o, p, q, r, s, t, u, v, z],
	lists:foreach(fun(Node) -> ring:add_node(R, Node, 100) end, 
						NodeList ),

	N = 100000,
	{Res, Stats} = util:benchmark( ?MODULE, get_node_test, [R, N, NodeList], 3 ),
 	io:format( "Res: ~p~n", [Res] ),
	lists:foreach( fun({Name, Value}) ->
					   io:format( "~p:\t ~10.3f ms~n", [Name, Value/1000] )
				   end, Stats ), 
	
	% Test get physical node
	lists:foreach( fun(Node) -> io:format( "~p  -->  ~p~n", [Node, get_next_physical_node(R, Node)]) end, 
				   NodeList ),
	
	% Test get preference list
	lists:foreach( fun(Name) -> io:format( "~p  -->  ~p~n", [Name, get_preference_list(R, Name, 3)]) end, 
				   ["hola", "hello", "ciao" ] ),
	ok.

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

