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
-export([txsend/4]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
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
            % lager:debug("---> ~w", [Data2]),
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
    Dict2 = case dict:find(MAC, Dict) of
        {ok, {Host, Port, Version}} ->
            Dict;
        _Else ->
            lager:info("Gateway ~w at ~w:~w", [MAC, Host, Port]),
            dict:store(MAC, {Host, Port, Version}, Dict)
    end,
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


rxpk(MAC, PkList) ->
    PkList2 = lists:map(fun(Pk) -> parse_rxpk(Pk) end, PkList),
    % due to reflections the gateways may have received the same frame twice
    Unique = remove_duplicates(PkList2, []),
    % handle the frames sequentially
    lists:foreach(fun({RxQ, PHYPayload}) ->
        lorawan_handler:handle_rxpk(MAC, RxQ, PHYPayload) end, Unique).

remove_duplicates([{RxQ, PHYPayload} | Tail], Unique) ->
    % check if the first element is duplicate
    case lists:keytake(PHYPayload, 2, Tail) of
        {value, {RxQ2, PHYPayload}, Tail2} ->
            % select element of a better quality and re-check for other duplicates
            if
                RxQ#rxq.rssi >= RxQ2#rxq.rssi ->
                    remove_duplicates([{RxQ, PHYPayload} | Tail2], Unique);
                true -> % else
                    remove_duplicates([{RxQ2, PHYPayload} | Tail2], Unique)
            end;
        false ->
            remove_duplicates(Tail, [{RxQ, PHYPayload} | Unique])
    end;
remove_duplicates([], Unique) ->
    Unique.

status(MAC, Pk) ->
    S = ?to_record(stat, Pk),
    case lorawan_mac:process_status(MAC, S) of
        ok -> ok;
        {error, Error} ->
            lager:error("ERROR: ~w", [Error])
    end.

txsend(Pid, Gateway, TxQ, PHYPayload) ->
    % TX only supported on radio A
    Pk = [{txpk, build_txpk(TxQ#txq{rfch=Gateway#gateway.tx_rfch}, PHYPayload)}],
    % lager:debug("<--- ~w", [Pk]),
    gen_server:cast(Pid, {send, Gateway#gateway.mac, jsx:encode(Pk)}).

parse_rxpk(Pk) ->
    Data = base64:decode(proplists:get_value(data, Pk)),
    case proplists:get_value(modu, Pk) of
        <<"LORA">> ->
            RxQ = ?to_record(rxq, Pk),
            {RxQ#rxq{erlst=erlang:monotonic_time(milli_seconds)}, Data}
    end.

build_txpk(TxQ, Data) ->
    ?to_proplist(txq, TxQ) ++
        [{imme, false}, {modu, <<"LORA">>}, {ipol, true}, {size, byte_size(Data)}, {data, base64:encode(Data)}].

% end of file
