%%% -------------------------------------------------------------------
%%% Author  : mat
%%% Description :
%%%
%%% Created : 17/mar/2009
%%% -------------------------------------------------------------------
-module(distmap_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([]).

-export( [start_link/1, init/1] ).

start_link( Conf ) ->
    supervisor:start_link( {local, ?MODULE}, ?MODULE, [Conf]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init( Conf ) ->
    Config = { dm_config, {dm_config, start_link, [Conf] }, 
            permanent, 2000, worker, [dm_config] },
    Log = { dm_log, {dm_log, start_link, [] }, 
            permanent, 2000, worker, [dm_log] },
    Finder = { dm_finder, {dm_finder, start_link, [] }, 
            permanent, 2000, worker, [dm_finder] },
    Membership = { dm_membership, {dm_membership, start_link, [] }, 
            permanent, 2000, worker, [dm_membership] },
    Monitor = { dm_monitor, {dm_monitor, start_link, [] }, 
            permanent, 2000, worker, [dm_monitor] },
    
    { ok, { {one_for_one, 5, 10}, [Config, Log, Finder, Membership, Monitor ]} }.


