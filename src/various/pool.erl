%% Author: mat
%% Created: 10/mar/2009
%% Description: TODO: Add description to pool
-module(pool).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([create/1, close/1, add/2, test/0]).

%%
%% API Functions
%%

create( Size ) -> 
	PidList = create_list( Size ),
	spawn( fun() -> pool(PidList, queue:new()) end ).

close( Pool ) ->
	Pool ! {close}.

add( Pool, Fun ) ->
	Pool ! {start_job, Fun }.

%%
%% Local Functions
%%
create_list(0) -> [];
create_list(Size) ->
	Pid = spawn( fun wait_job/0 ),
	[Pid|create_list(Size-1)].

pool( PidList, JobQueue ) ->
	receive
		{start_job, Fun} ->
			case PidList of
				[] -> % No process available 
					NewQueue = queue:in( Fun, JobQueue ),
					pool( PidList, NewQueue );
				
				[FirstPid|Rest] ->
					FirstPid ! {job, self(), Fun},
					pool( Rest, JobQueue )
			end;
		
		{end_job, Pid} ->
			Empty = queue:is_empty( JobQueue ),
			if Empty =:= false ->
				   	{{value, Fun}, Q2} = queue:out_r( JobQueue ),
					Pid ! {job, self(), Fun},
					pool( PidList, Q2 );
				
				true ->
					pool( [Pid|PidList], JobQueue )
			end;
		
		{close} ->
			lists:map( fun(Pid) -> Pid ! {stop} end, PidList ) 
	end.

wait_job() ->
	receive
		{job, From, Fun} ->
			Fun(),
			From ! {end_job, self()},
			wait_job();
			
		{stop} ->
			ok
	end.

wait( N ) ->
	receive
	after N*1000 -> ok
	end.

test() ->
	Pool = pool:create( 5 ),
	lists:map( fun(I) -> 
			pool:add( Pool, fun() -> 
								io:format( "~p : ~p start~n", [self(), I] ), 
								wait( 2 ),
								io:format( "~p : ~p end~n", [self(), I] )
					        end )
		 end, 
		 lists:seq( 1, 100 ) ),
	
	pool:close( Pool ),
	ok.
