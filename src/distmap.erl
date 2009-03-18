%% Author: mat
%% Created: 17/mar/2009
%% Description: TODO: Add description to distmap
-module(distmap).

-export( [start/0, start/1, start/2, stop/1 ]).

-include( "distmap.hrl" ).

-define( OptSpec, [ {help, h, boolean},
	        	    {version, v, boolean},
		            {debug, d, boolean}, 
					{name, n, string},
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
	Help = getoptions:get_opt( help, Conf ), 
	if Help == true ->
		   usage(),
		   erlang:halt( 0 );
	   true -> ok
	end,
	
	Version = getoptions:get_opt( version, Conf ), 
	if Version == true ->
		   version(),
		   erlang:halt( 0 );
	   true -> ok
	end,
	
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
	
	{ok, SupPid} =
	case distmap_sup:start_link( Conf ) of
		{ok, Pid} ->
			?INFO( "Distmap application started" ),
			{ok, Pid};
    	Error -> 
			io:format( "~nError starting distmap: ~n~p~n", [Error] ),
			erlang:halt(1)
	end,
	
	% All module are now started
	
	NodeName = dm_config:get( name ),
	if NodeName == none ->
			?ERROR( "Please select a name for this node, using the --name switch" ),
			util:sleep( 1 ),
			erlang:halt( 1 );
		true -> ok
	end,
	NodeName2 =  case string:str( NodeName, "@" ) of
		0 ->
			IP = dm_finder:get_external_ip(),
			Domain = util:format_addr( IP ),
			NodeName ++ "@" ++ Domain;
	    _ -> 
			NodeName
	end,
		
	?INFO_( "NodeName: '~s'", [NodeName2] ),
	net_kernel:start( [?L2A(NodeName2), longnames] ),
	
	dm_finder:announce_myself( dm_node ),
	
	{ok, SupPid}.

%% --------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%% --------------------------------------------------------------------
stop( _ ) ->
	exit( whereis(distmap_sup), shutdown ).

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

version() -> 
	io:format( "DistMap 1.0-a1~n"
	 "Copyright (c) 2009 Matteo Merli <mm@merlimat.org>~n" 
	 "This is free software; see the source for copying conditions.  There is NO~n"
	 "warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.~n~n" ).


