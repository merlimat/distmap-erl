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

new() -> empty.

is_empty( Ring ) when Ring =:= empty -> true;
is_empty( _ ) -> false.

put(K, V, T) ->
	{_,L,K1,V1,R} = put1(K, V, T),
	{b,L,K1,V1,R}.				%setelement(1, b, T1).

put1(K, V, empty) -> {r,empty,K,V,empty};
put1(K, V, {C,Left,K1,V1,Right}) when K < K1 ->
    lbalance(C, put1(K, V, Left), K1, V1, Right);
put1(K, V, {C,Left,K1,V1,Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, put1(K, V, Right));
put1(K, V, {C,L,_,_,R}) ->
    {C,L,K,V,R}.

get_node(Key, Ring) -> 
	ok.	

get_node1(K, {_,Left,K1,Val,_}, Ring) when K < K1 ->
	io:format("K1: ~p -- Left: ~p~n", [K1, Left]),
	case Left of
		{_, _, K2, _, _} ->
			if K =< K2 -> 
				   get_node1( K, Left, Ring );
			   true -> 
				   Val
			end;

		empty -> Val
	end;
get_node1( K, {_,_,K1,Val,Right}, Ring ) when K > K1 ->
	io:format("K1: ~p -- Right: ~p~n", [K1, Right]),
	case Right of
		{_, _, K2, _, _} ->
			if K >= K2 -> 
				   get_node1( K, Right, Ring );
			   true -> 
				   Val
			end;

		empty -> Val
	end;
get_node1(_, {_,_,_,Val,_}, _) -> Val.

nodes( T ) -> nodes( T, [] ).
nodes( empty, Tail ) -> Tail;
nodes( {_,L,_K,V,R}, Tail ) ->
	nodes( L, [V|nodes(R, Tail)] ).

%%
%% Local Functions
%%

%% lbalance(Colour, Left, Key, Val, Right).
%% rbalance(Colour, Left, Key, Val, Right).
%% Balance a tree afer (possibly) adding a node to the left/right.
lbalance(b, {r,{r,A,Xk,Xv,B},Yk,Yv,C}, Zk, Zv, D) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
lbalance(b, {r,A,Xk,Xv,{r,B,Yk,Yv,C}}, Zk, Zv, D) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
lbalance(C, A, Xk, Xv, B) -> {C,A,Xk,Xv,B}.

rbalance(b, A, Xk, Xv, {r,{r,B,Yk,Yv,C},Zk,Zv,D}) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
rbalance(b, A, Xk, Xv, {r,B,Yk,Yv,{r,C,Zk,Zv,D}}) ->
    {r,{b,A,Xk,Xv,B},Yk,Yv,{b,C,Zk,Zv,D}};
rbalance(C, A, Xk, Xv, B) -> {C,A,Xk,Xv,B}.

test() ->
	R = new(),
	R1 = put( 1, "one", R ),
	R2 = put( 2, "two", R1 ),
	R3 = put( 5, "five", R2 ),
	R4 = put( 20, "twenty", R3 ),
	io:format( "R4: ~p~n", [R4] ),
	io:format( "Keys: ~p~n", [nodes(R4)] ),
	%lists:map( fun(X) ->
	%		     io:format( "~p: ~p~n", [X, get_node(X, R4)] )
	%	       end, nodes(R4) ),
	
	io:format( "0: ~p~n", [get_node(0, R4)] ),
	io:format( "7: ~p~n", [get_node(7, R4)] ),
	ok.


	
