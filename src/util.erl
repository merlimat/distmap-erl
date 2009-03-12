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
-export([format_addr/2, unjoin/2]).

%%
%% API Functions
%%

format_addr( {A,B,C,D}, Port ) -> 
	lists:flatten( io_lib:format( "~p.~p.~p.~p:~p", [A,B,C,D,Port]) ).

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
