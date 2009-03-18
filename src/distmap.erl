%% Author: mat
%% Created: 17/mar/2009
%% Description: TODO: Add description to distmap
-module(distmap).

-export( [start/0, start/1, start/2, stop/1 ]).

-include( "distmap.hrl" ).

-define( OptSpec, [ {help, h, boolean},
	        	    {version, v, boolean},
		            {debug, d, boolean}, 
					{name, string},
					{shell, s, boolean}
	        ] ).

-define( Defaults,  [ {debug, false} ] ).

-define( ARGS_PROC, args_proc ).

%% ====================================================================!
%% External functions
%% ====================================================================!
start() ->
	start_app( ?Defaults ).

start( Args ) ->
	{Conf, _} = process_args( Args ),
	start_app( Conf ).

start_app( Conf ) ->
	% Start a process that will hold the configuration
	% waiting for the "app" that will retrieve them.
	register( ?ARGS_PROC, spawn( fun() -> 
								    receive 
										{get_args,From} -> 
											From ! {?ARGS_PROC, Conf}
									end
								 end ) ),
	application:start( distmap ).

start( _Type, _StartArgs) ->
	?ARGS_PROC ! {get_args, self()},
	Conf = receive
			   {?ARGS_PROC, Args} -> Args
		   end,
	
	io:format( "Conf: ~p~n", [Conf] ),
	
	case distmap_sup:start_link() of
		{ok, Pid} ->
			?INFO( "Distmap application started" ),
			{ok, Pid};
    	Error -> 
			io:format( "~nError starting distmap: ~n~p~n", [Error] ),
			erlang:halt(1)
	end.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop( _Pid ) ->
	?INFO( "Stopping distmap application" ).

%% ====================================================================
%% Internal functions
%% ====================================================================

process_args( Args ) ->
	StrArgs = lists:map( fun(X) -> 
							S = atom_to_list(X),
							re:replace( S, "\\?", "\\-", 
										[{return, list}, global])
						 end, Args ),
	try
		getoptions:extract_options(StrArgs, ?OptSpec, ?Defaults)
	catch 
		_Reason -> 
			usage(), 
			erlang:halt( 1 )
	end.

usage() ->
	io:format( "distmap [options]~n"
		"   --help                      Print usage message~n"
   		"   --version                   Print version message~n"
		"   -d, --debug                 Print debug output~n"
		"   -c, --config <file>         Use alternate configuration file~n"
		"~n" ).

