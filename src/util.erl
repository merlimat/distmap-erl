%% Author: mat
%% Created: 11/mar/2009
%% Description: TODO: Add description to util
-module(util).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([for/3, format_addr/2, unjoin/2, benchmark/4]).

%%
%% API Functions
%%

for( I, N, _Fun ) when I > N-> ok;
for( I, N, Fun ) ->
	Fun( I ), 
	for( I+1, N, Fun ).

format_addr( {A,B,C,D}, Port ) -> 
	lists:flatten( io_lib:format( "~p.~p.~p.~p:~p", [A,B,C,D,Port]) ).


benchmark( Mod, Fun, Args, N ) when N > 0 ->
    L = test_loop( Mod, Fun, Args, N, []),
    Length = length(L),
    {Min,_} = lists:min(L),
    {Max,_} = lists:max(L),
    {Med,Res} = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun({X,_}, Sum) -> X + Sum end, 0, L) / Length),
	StdDev = round( math:sqrt( 
			   lists:foldl( fun({X,_}, Sum) -> math:pow( X - Avg, 2) + Sum end, 0, L ) 
			   / Length ) ),
	Stats = [ {min, Min}, {max, Max}, {med, Med}, {avg, Avg}, {stddev, StdDev}],
    {Res, Stats}.

test_loop( _Mod, _Fun, _Args, 0, List ) ->
    List;
test_loop( Mod, Fun, Args, N, List ) ->
    Res = timer:tc( Mod, Fun, Args ),
    test_loop( Mod, Fun, Args, N - 1, [Res|List] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unjoin(String, []) ->
     unjoin0(String);
unjoin(String, [Sep]) when is_integer(Sep) ->
     unjoin1(String, Sep);
unjoin(String, [C1,C2|L]) when is_integer(C1), is_integer(C2) ->
     unjoin2(String, C1, C2, L).

%%
%% Local Functions
%%

%% Split a string at "", which is deemed to occur _between_
%% adjacent characters, but queerly, not at the beginning
%% or the end.

unjoin0([C|Cs]) ->
     [[C] | unjoin0(Cs)];
unjoin0([]) ->
     [].

%% Split a string at a single character separator.

unjoin1(String, Sep) ->
     unjoin1_loop(String, Sep, "").

unjoin1_loop([Sep|String], Sep, Rev) ->
     [lists:reverse(Rev) | unjoin1(String, Sep)];
unjoin1_loop([Chr|String], Sep, Rev) ->
     unjoin1_loop(String, Sep, [Chr|Rev]);
unjoin1_loop([], _, Rev) ->
     [lists:reverse(Rev)].

%% Split a string at a multi-character separator
%% [C1,C2|L].  These components are split out for
%% a fast match.

unjoin2(String, C1, C2, L) ->
     unjoin2_loop(String, C1, C2, L, "").

unjoin2_loop([C1|S = [C2|String]], C1, C2, L, Rev) ->
     case unjoin_prefix(L, String)
       of no   -> unjoin2_loop(S, C1, C2, L, [C1|Rev])
        ; Rest -> [lists:reverse(Rev) | unjoin2(Rest, C1, C2, L)]
     end;
unjoin2_loop([Chr|String], C1, C2, L, Rev) ->
     unjoin2_loop(String, C1, C2, L, [Chr|Rev]);
unjoin2_loop([], _, _, _, Rev) ->
     [lists:reverse(Rev)].

unjoin_prefix([C|L], [C|S]) -> unjoin_prefix(L, S);
unjoin_prefix([],    S)     -> S;
unjoin_prefix(_,     _)     -> no.
