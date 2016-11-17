%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% Gateway interface for the https://github.com/Lora-net/packet_forwarder
% Supports protocol v2.2.0
% See https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT
%
-module(lorawan_iface_forwarder).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([txsend/5]).

-include("lorawan.hrl").

-record(state, {sock, pulladdr}).

start_link(Port) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    process_flag(trap_exit, true),
    {ok, Socket} = gen_udp:open(Port, [binary]),
    {ok, #state{sock=Socket, pulladdr=dict:new()}}.

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast({send, MAC, Packet}, #state{sock=Socket, pulladdr=Dict}=State) ->
    case dict:find(MAC, Dict) of
        {ok, {Host, Port, Version}} ->
            ID = crypto:strong_rand_bytes(2),
            % PULL RESP
            gen_udp:send(Socket, Host, Port, <<Version, ID/binary, 3, Packet/binary>>);
        error ->
            lager:info("Downlink request ignored. Gateway ~w not connected.", [MAC])
    end,
    {noreply, State}.

% PUSH DATA
handle_info({udp, Socket, Host, Port, <<Version, Token:16, 0, MAC:8/binary, Data/binary>>}, #state{sock=Socket}=State) ->
    case jsx:is_json(Data) of
        true ->
            Data2 = jsx:decode(Data, [{labels, atom}]),
            lists:foreach(
                fun ({rxpk, Pk}) -> rxpk(MAC, Pk);
                    ({stat, Pk}) -> status(MAC, Pk)
                end, Data2),
            % PUSH ACK
            gen_udp:send(Socket, Host, Port, <<Version, Token:16, 1>>);
        false ->
            lager:error("Ignored PUSH_DATA: JSON syntax error")
    end,
    {noreply, State};

% PULL DATA
handle_info({udp, Socket, Host, Port, <<Version, Token:16, 2, MAC:8/binary>>}, #state{sock=Socket, pulladdr=Dict}=State) ->
    Dict2 = dict:store(MAC, {Host, Port, Version}, Dict),
    % PULL ACK
    gen_udp:send(Socket, Host, Port, <<Version, Token:16, 4>>),
    {noreply, State#state{pulladdr=Dict2}};

% TX ACK
handle_info({udp, Socket, _Host, _Port, <<_Version, _Token:16, 5, _MAC:8/binary>>}, #state{sock=Socket}=State) ->
    % no error occured
    {noreply, State};

% TX ACK
handle_info({udp, Socket, _Host, _Port, <<_Version, _Token:16, 5, MAC:8/binary, Data/binary>>}, #state{sock=Socket}=State) ->
    case jsx:is_json(Data) of
        true ->
            Data2 = jsx:decode(Data, [{labels, atom}]),
            Ack = proplists:get_value(txpk_ack, Data2),
            Error = proplists:get_value(error, Ack),
            lager:error("Transmission via ~w failed: ~s", [MAC, Error]);
        false ->
            lager:error("Ignored PUSH_DATA: JSON syntax error")
    end,
    {noreply, State};

% something strange
handle_info({udp, _Socket, _Host, _Port, _Msg}, State) ->
    {noreply, State};

% handler termination
handle_info({'EXIT', _FromPid, _Reason}, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


rxpk(_MAC, []) -> ok;
rxpk(MAC, [Pk|More]) ->
    {RxQ, RF, PHYPayload} = parse_rxpk(Pk),
    lorawan_handler:handle_rxpk(MAC, RxQ, RF, PHYPayload),
    rxpk(MAC, More).

status(MAC, Pk) ->
    S = ?to_record(stat, Pk),
    case lorawan_mac:process_status(MAC, S) of
        ok -> ok;
        {error, Error} ->
            lager:error("ERROR: ~w", [Error])
    end.

txsend(Pid, MAC, Time, RF, PHYPayload) ->
    % TX only supported on radio A
    Pk = jsx:encode([{txpk, build_txpk(#txq{tmst=Time, rfch=0, powe=14}, RF, PHYPayload)}]),
    gen_server:cast(Pid, {send, MAC, Pk}).

parse_rxpk(Pk) ->
    Data = base64:decode(proplists:get_value(data, Pk)),
    case proplists:get_value(modu, Pk) of
        <<"LORA">> -> {?to_record(rxq, Pk), ?to_record(rflora, Pk), Data}
    end.

build_txpk(TxQ, RF, Data) ->
    ?to_proplist(txq, TxQ) ++ ?to_proplist(rflora, RF) ++ 
        [{imme, false}, {modu, <<"LORA">>}, {ipol, true}, {size, byte_size(Data)}, {data, base64:encode(Data)}].

% end of file
