%% Author: mat
%% Created: 05/mar/2009
%% Description: TODO: Add description to monitor
-module(dm_monitor).

%%
%% Include files
%%

-include( "distmap.hrl" ).

%%
%% Exported Functions
%%
-export([start_monitor/1, end_monitor/0, ping_proc/1]).

% gen_server callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2, 
         handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], _Opts = []).

start_monitor( Node ) ->
    gen_server:cast( ?MODULE, {start_monitor, Node} ).

end_monitor() ->
    gen_server:cast( ?MODULE, end_monitor ).

%% ====================================================================
%% Server functions
%% ====================================================================

init([]) ->
    {ok, {none, none}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% ====================================================================

handle_cast( {start_monitor, Node}, {PingProc,PrevNode} ) ->
    if PrevNode == Node ->
           {noreply, {PingProc,Node}};
       true ->
           if PingProc /= none -> exit( PingProc, kill );
                true -> ok
           end,
           Pid = spawn( ?MODULE, ping_proc, [Node] ),
           ?INFO_( "Monitoring node: ~p", [Node] ),
           {noreply, {Pid, Node}}   
    end;

handle_cast( end_monitor, {PingProc,_Node} ) ->
    ?DEBUG( "Stop monitoring, are we alone?" ),
    case PingProc of
        none -> ok;
        Pid -> 
            exit( Pid, kill )
    end,
    {noreply, none}.

%% ====================================================================

handle_info(_Info, State) ->
    ?DEBUG( "Got Info" ),
    {noreply, State}.

terminate(Reason, _State) ->
    ?DEBUG_( "Stopping: ~p", Reason ).

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

ping_proc( Node ) when is_atom(Node) ->
    case catch gen:call( {net_kernel, Node}, '$gen_call',
                         {is_auth, node()}, 1000 ) of
        {ok, yes} ->
            util:sleep( 5 ),
            ping_proc( Node );
        _ ->
            ?DEBUG_( "Node is down: ~p", [Node] ),
            dm_membership:node_is_down( Node )
    end.
