%% Author: mat
%% Created: 11/mar/2009
%% Description: TODO: Add description to config
-module(dm_config).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

-include( "distmap.hrl" ).

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, get/1, get_bool/1, get_addr/1]).

%% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, 
          terminate/2, code_change/3 ]).

-define( Defaults, [
        {debug, "false"},
        {log_file, none}, % "logs/distmap.log"}, 
        {name, none}, 
        {multicast_channel, "226.0.0.1:20000" }     
    ] ).

%% ====================================================================
%% External functions
%% ====================================================================
start_link( [CmdLineConf] ) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [CmdLineConf], _Opts = []).

get( Key ) when is_atom(Key) ->
    gen_server:call( ?MODULE, {get, Key} ).

get_bool( Key ) ->
    ?MODULE:get( Key ) == "true".

get_addr( Key ) ->
    util:parse_ip_address( ?MODULE:get( Key ) ).

%% ====================================================================
%% Server functions
%% ====================================================================

init( [CmdLineConf] ) ->
    Table = ets:new(?MODULE,[set]),
    lists:foreach( fun( X ) ->
                        ets:insert(Table, X)
                   end, 
                   ?Defaults ),
    lists:foreach( fun( {Key, Value} ) ->
                        V2 = if is_atom(Value) -> atom_to_list(Value);
                                true -> Value
                             end,
                        ets:insert(Table, {Key, V2})
                   end, 
                   CmdLineConf ),
    {ok, Table}.

handle_call( {get, Key}, _From, Table ) ->
    [{Key,Value}] = ets:lookup( Table, Key ),
    {reply, Value, Table};

handle_call( _Request, _From, State ) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

