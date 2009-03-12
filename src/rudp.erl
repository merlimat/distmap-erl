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
%%         2 : ...

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

-define( MTU, 1420 ).

create( Port ) ->
	create( {0,0,0,0}, Port, [] ).

create( Address, Port, Options ) ->
	log:start(),
	gen_server:start_link ( ?MODULE, [Address, Port, Options], []).

send( Pid, Peer, Data ) ->
	Bin = term_to_binary(Data),
	gen_server:cast(Pid, {send, Peer, Bin}).

%% ====================================================================
%% Server functions
%% ====================================================================

-record(state, { socket, nounce_cnt=0 }).
-record(pkt, {
		syn = 0,
		ack = 0,
		last = 0,
		nounce,
		sequence = 1,
		ack_seq = 0
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
	{PktList, NewState} = prepare_data( Data, State ),
	lists:foreach( fun(Pkt) ->
			         ok = gen_udp:send( State#state.socket, Addr, Port, Pkt )
		           end, PktList ),
	{noreply, NewState}.

handle_info( {udp, Socket, IP, Port, Packet},
             State=#state{ socket = Socket }) ->
	?DEBUG_( "Got packet from ~s", util:format_addr(IP, Port) ),
	{Pkt,_Data} = read_packet( Packet ),
	io:format( "~s~n", [format_pkt(Pkt)] ),
	{ noreply, State };

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

prepare_data( Data, State ) when byte_size(Data) =< ?MTU ->
	Nounce = State#state.nounce_cnt + 1,
	Pkt = new_packet( #pkt{ syn=1, last=1, nounce=Nounce }, Data ),
	{ [Pkt], State#state{ nounce_cnt = Nounce } };

prepare_data( Data, State ) ->
	Nounce = State#state.nounce_cnt + 1,
	<<DataFirst:?MTU/binary, DataRest/binary>> = Data,
	FirstPkt = new_packet( #pkt{ syn=1, nounce=Nounce }, DataFirst ),
	Rest = prepare_data_cont( 2, DataRest, Nounce ),
	{ [FirstPkt|Rest], State#state{ nounce_cnt = Nounce } }.

prepare_data_cont( Seq, Nounce, Data ) ->
	List = prepare_data_cont( Seq, Nounce, Data, [] ),
	lists:reverse( List ).

prepare_data_cont( Seq, Nounce, Data, Acc ) when byte_size(Data) =< ?MTU ->
	Pkt = new_packet( #pkt{ last=1, sequence=Seq, nounce=Nounce }, Data ), 
	[Pkt|Acc];
prepare_data_cont( Seq, Nounce, Data, Acc ) ->
	<<DataCurr:?MTU/binary, DataRest/binary>> = Data,
	Pkt = new_packet( #pkt{ sequence=Seq, nounce=Nounce }, DataCurr ),
	prepare_data_cont( Seq+1, Nounce, DataRest, [Pkt|Acc] ).

new_packet( Header, DataFragment ) ->
 	<< (Header#pkt.syn):1, 
 	   (Header#pkt.ack):1,
	   (Header#pkt.last):1,
	   0:5, % pad with zeros
 	   (Header#pkt.nounce):24,
	   (Header#pkt.sequence):16,
 	   (Header#pkt.ack_seq):16,
 	   DataFragment/binary
 	>>.
	   

%% Packet reading

read_packet( Bin ) ->
	<< SYN:1, 
 	   ACK:1,
	   LAST:1,
	   0:5, % pad with zeros
 	   Nounce:24,
	   Sequence:16,
 	   AckSeq:16,
 	   DataFragment/binary
 	>> = Bin,
	{ #pkt{ syn = SYN, ack = ACK, last = LAST,
		  nounce = Nounce, sequence = Sequence, 
		  ack_seq = AckSeq }, DataFragment }.

format_pkt( Pkt=#pkt{} ) ->
	lists:flatten( io_lib:format( 
		"[SYN:~p ACK:~p LAST:~p Nounce:~p Seq:~p Ack:~p]", 
			[Pkt#pkt.syn, Pkt#pkt.ack, Pkt#pkt.last,
			 Pkt#pkt.nounce, Pkt#pkt.sequence, Pkt#pkt.ack_seq]) ).
