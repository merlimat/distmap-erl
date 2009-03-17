%% Author: mat
%% Created: 12/mar/2009
%% Description: TODO: Add description to rudp
-module(rudp).

%%
%%   0      7 8     15 16    23 24    31
%%  +--------+--------+--------+--------+
%%  |  Flags |          Nounce          |
%%  +--------+--------+--------+--------+
%%  |  Pkt Sequence   |   Ack Sequence  |
%%  +--------+--------+--------+--------+
%%  |          data octets ...
%%  +---------------- ...
%%

%% Flags:  0 : SYN - Active on first packet
%%         1 : ACK - The ACK field is valid
%%         2 : LAST - The packet is last in sequence
%%         3 : REQ - The packet is last in sequence
%%         4 : RESP - The packet is last in sequence
%%         5 : ...
%%         6 : ...
%%         7 : ...

%%
%% Include files
%%

-behaviour(gen_server).

-include( "distmap.hrl" ).

%%
%% Exported Functions
%%
-export([create/1, create/3, send/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% API Functions
%%

-define( MTU, 1412 ).

create( Port ) when is_integer(Port) ->
	create( {0,0,0,0}, Port, [] ).

create( Address, Port, Options ) ->
	log:start(),
	gen_server:start_link ( ?MODULE, [Address, Port, Options], []).

send( Pid, Peer, Data ) when is_binary(Data) ->
	gen_server:cast(Pid, {send, Peer, Data});
send( Pid, Peer, Data ) ->
	Bin = term_to_binary(Data),
	gen_server:cast(Pid, {send, Peer, Bin}).

%% ====================================================================
%% Server functions
%% ====================================================================

-record(state, { socket, 
				 nounce_cnt = 0, 
				 operations = dict:new() }).

-record(operation, {
				nounce,
				sender = remote,
				pkt_list = [], 
				last_ack = 0
		} ).

-record(pkt, {
		syn = 0,
		ack = 0,
		last = 0,
		req = 0,
		resp = 0,
		nounce,
		sequence = 1,
		ack_seq = 0,
		data
	} ).

init([Addr, Port, _Options]) ->
	process_flag( trap_exit, true ),
	
  	Opts = [ { active, true },
    	     { ip, Addr },
             { reuseaddr, true },
           binary ],

	{ok, Socket} = gen_udp:open( Port, Opts ),
	{ok, EffPort} = inet:port( Socket ),
	?DEBUG_( "Listening on ~s", util:format_addr(Addr, EffPort) ),
	{ok, #state{ socket = Socket }}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast({send, {Addr,Port}, Data}, State) ->
	{PktList, NewState} = prepare_data( request, Data, State ),
	lists:foreach( fun(Pkt) ->
			         ok = gen_udp:send( State#state.socket, Addr, Port, Pkt )
		           end, PktList ),
	{noreply, NewState}.

handle_info( {udp, Socket, IP, Port, Packet},
             State=#state{ socket = Socket }) ->
	Pkt = read_packet( Packet ),
	?DEBUG_( "Got packet from ~s - ~s",	[util:format_addr(IP, Port), format_pkt(Pkt)] ),
	{ok, NewState} = process_fragment( {IP,Port}, Pkt, State ),
	{ noreply, NewState };

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, State) ->
	?DEBUG_( "Terminating rudp. Reason: ~p", [Reason] ),
	gen_udp:close (State#state.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Local Functions
%%

%% Packet creation

prepare_data( request, Data, State ) ->
	prepare_data( 1, Data, State );
prepare_data( response, Data, State ) ->
	prepare_data( 0, Data, State );

prepare_data( Req, Data, State ) when byte_size(Data) =< ?MTU ->
	Nounce = State#state.nounce_cnt + 1,
	Resp = 1 - Req,
	Pkt = new_packet( #pkt{ syn=1, last=1, req=Req, 
							resp=Resp, nounce=Nounce, data=Data } ),
	{ [Pkt], State#state{ nounce_cnt = Nounce } };

prepare_data( Req, Data, State ) ->
	Nounce = State#state.nounce_cnt + 1,
	Resp = 1 - Req,
	{DataFirst, DataRest} = split_binary(Data, ?MTU),
	FirstPkt = new_packet( #pkt{ syn=1, req=Req, 
								 resp=Resp, nounce=Nounce, data=DataFirst } ),
	Rest = prepare_data_cont( Req, 2, Nounce, DataRest ),
	{ [FirstPkt|Rest], State#state{ nounce_cnt = Nounce } }.

prepare_data_cont( Req, Seq, Nounce, Data ) ->
	List = prepare_data_cont( Req, Seq, Nounce, Data, [] ),
	lists:reverse( List ).

prepare_data_cont( Req, Seq, Nounce, Data, Acc ) when byte_size(Data) =< ?MTU ->
	Resp = 1 - Req,
	Pkt = new_packet( #pkt{ last=1, sequence=Seq, req=Req, 
							resp=Resp, nounce=Nounce, data=Data } ), 
	[Pkt|Acc];
prepare_data_cont( Req, Seq, Nounce, Data, Acc ) ->
	Resp = 1 - Req,
	{DataCurr, DataRest} = split_binary(Data, ?MTU),
	Pkt = new_packet( #pkt{ sequence=Seq, req=Req, 
							resp=Resp, nounce=Nounce, data=DataCurr } ),
	prepare_data_cont( Req, Seq+1, Nounce, DataRest, [Pkt|Acc] ).

new_packet( Pkt ) ->
 	<< (Pkt#pkt.syn):1, 
 	   (Pkt#pkt.ack):1,
	   (Pkt#pkt.last):1,
	   (Pkt#pkt.req):1,
	   (Pkt#pkt.resp):1,
	   0:3, % pad with zeros
 	   (Pkt#pkt.nounce):24,
	   (Pkt#pkt.sequence):16,
 	   (Pkt#pkt.ack_seq):16,
 	   (Pkt#pkt.data)/binary
 	>>.
	   

%% Packet reading

read_packet( Bin ) ->
	<< SYN:1, 
 	   ACK:1,
	   LAST:1,
	   REQ:1,
	   RESP:1,
	   0:3, % pad with zeros
 	   Nounce:24,
	   Sequence:16,
 	   AckSeq:16,
 	   DataFragment/binary
 	>> = Bin,
	#pkt{ syn = SYN, ack = ACK, req = REQ, resp = RESP, last = LAST,
		  nounce = Nounce, sequence = Sequence, 
		  ack_seq = AckSeq, data=DataFragment }.

format_pkt( Pkt=#pkt{} ) ->
	lists:flatten( io_lib:format( 
		"[SYN:~p ACK:~p LAST:~p REQ:~p RESP:~p Nounce:~p Seq:~p Ack:~p Size:~p]", 
			[Pkt#pkt.syn, Pkt#pkt.ack, Pkt#pkt.last,
			 Pkt#pkt.req, Pkt#pkt.resp,
			 Pkt#pkt.nounce, Pkt#pkt.sequence, 
			 Pkt#pkt.ack_seq, byte_size(Pkt#pkt.data)]) ).

%% Incoming packet processing 

process_fragment( From, Pkt=#pkt{req=1}, State ) ->
	Dict = State#state.operations,
	Nounce = Pkt#pkt.nounce,
	Key = {From, Nounce},
	Operation = case dict:find(Key, Dict) of 
							{ok, Value} -> Value; 
							error  ->  #operation{nounce=Nounce}
					   end,
	
	PktList = sort_packets( [Pkt|Operation#operation.pkt_list] ),
	io:format( "Is pkt_list complete? ~p~n", [is_pkt_list_complete(PktList)] ),
	Operation1 = Operation#operation{pkt_list=PktList},
	NewDict = dict:store( Key, Operation1, Dict ),
	{ok, State#state{operations=NewDict}};

process_fragment( From, Pkt=#pkt{resp=1}, State ) ->
	{ok, State}.

process_ack( Nounce, AckSeq, State ) ->
	ok.

% Sort a list of packets according to sequence number
sort_packets( PktList ) ->
	lists:sort( fun(A,B) -> 
					if A#pkt.sequence > B#pkt.sequence -> false;
						true -> true
					end
				end, PktList ).

% Tells whether a packet list is complete 
is_pkt_list_complete( PktList ) ->
	is_pkt_list_complete(PktList, 1).

is_pkt_list_complete( [Pkt], N ) ->
	if Pkt#pkt.sequence == N -> 
		   if Pkt#pkt.last == 1 -> true;
		   	  true -> false
		   end;
	   true -> false
	end;
is_pkt_list_complete( [First|Rest], N ) ->
	if First#pkt.sequence == N ->
		   is_pkt_list_complete(Rest, N+1);
	   true -> false
	end.

