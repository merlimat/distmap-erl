%%% -------------------------------------------------------------------
%%% Author  : mat
%%% Description :
%%%
%%% Created : 11/mar/2009
%%% -------------------------------------------------------------------
-module(dm_log).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include( "distmap.hrl" ).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, log/5, log/6, test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], _Opts = []).

log( Type, Pid, File, Line, Format ) ->
	log( Type, Pid, File, Line, Format, none ).

log( Type, Pid, File, Line, Format, Args ) ->
	gen_server:cast(?MODULE, {log, Type, Pid, File, Line, Format, Args}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) ->
	Debug = dm_config:get_bool( debug ),
	case dm_config:get( log_file ) of
        none ->
            {ok, [{file,none}, {debug,Debug}] };
        File ->
            case file:open(File, [write, append]) of
                {ok, Fd} -> {ok, [{file, Fd}, {debug, Debug}]};
                Error    -> Error
            end
    end.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
	io:format( "Got call....~n" ),
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast( {log, Type, Pid, File, Line, Format, Args}, State ) ->
	case Type of 
		'DEBUG' ->
			case proplists:get_bool(debug,State) of
				false -> ok;
        		true  -> do_log( Type, Pid, File, Line, Format, Args, State ) 
    		end;
		
		_ -> 
			do_log( Type, Pid, File, Line, Format, Args, State )
	end,
    {noreply, State}.

handle_info(_Info, State) ->
	io:format( "Got Info....~n" ),
    {noreply, State}.

terminate(_Reason, State) ->
	io:format( "Stopping log~n" ),
    case proplists:get_value(file,State) of
        none -> ok;
        Fd   -> file:close(Fd)
    end.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

do_log( Type, Pid, File, Line, Format, Args, State ) ->
	{{Year,Month,Day}, {Hour,Minute,Second}} = erlang:localtime(),
    {_MegaSec, _Sec, Usec} = now(),
    Format2 =
        if
            is_list(Format) -> lists:flatten(Format);
            true          -> Format
        end,
	
	% Allow non-list singular argument
	IsString = if is_atom(Args) -> false;
				  true -> is_string(Args)
			   end,
	Args2 =
        if is_atom(Args) -> none;
		   IsString == true -> [Args];
		   is_list(Args)   -> Args;
           true            -> [Args]
        end,
	
	Data = case Args2 of 
			   none -> Format2;
			   List when is_list(List) ->
				   lists:flatten( io_lib:format( Format2, Args2 ) )
		   end,
	
    Buf = io_lib:format( 
			"~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w.~6..0w [~-7s] ~p (~s:~w) - ~s\n",
        [ Year, Month, Day, Hour, Minute, Second, Usec, 
		  Type, Pid, 
		  get_simple_name(File), 
		  Line, Data ]
    ),
	
    case proplists:get_value(file,State) of 
		none -> io:format(    "~s", [Buf]);
        Fd   -> io:format(Fd, "~s", [Buf])
    end.

get_simple_name( AbsolutePath ) ->
	[First|_] = lists:reverse( util:unjoin( AbsolutePath, "/" ) ),
	First.

is_string(X) when is_atom(X) -> false;
is_string(X) when is_tuple(X) -> false;
is_string([H|T]) ->
    if 0 =< H, 
	   H < 256, 
	   is_integer(H)  -> is_string(T);
	true -> false
    end;
is_string([]) -> true.


test() ->
	?ERROR( "Que pasa??" ),
	?INFO_( "Test: ~p", 1 ),

	?DEBUG_( "Prova: ~p ~p", [1, 2] ).
	
