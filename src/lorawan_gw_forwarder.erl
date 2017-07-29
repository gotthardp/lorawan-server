%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% Gateway interface for the https://github.com/Lora-net/packet_forwarder
% Supports protocol v2.2.0
% See https://github.com/Lora-net/packet_forwarder/blob/master/PROTOCOL.TXT
%
-module(lorawan_gw_forwarder).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

-record(state, {sock, tokens}).

start_link(PktFwdOpts) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [PktFwdOpts], []).

init([PktFwdOpts]) ->
    % We set the port to 0 because it is given in the Opts directly.
    % The port in the options takes precedence over the one in the first argument.
    case gen_udp:open(0, [binary | PktFwdOpts]) of
        {ok, Socket} ->
            {ok, #state{sock=Socket, tokens=maps:new()}};
        {error, Reason} ->
            lager:error("Failed to start the packet_forwarder interface: ~p", [Reason]),
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {stop, {error, unknownmsg}, State}.

handle_cast({send, {Host, Port, Version}, #request{tmst=UStamp}, DevAddr, TxQ, RFCh, PHYPayload},
        #state{sock=Socket, tokens=Tokens}=State) ->
    Pk = [{txpk, build_txpk(TxQ, RFCh, PHYPayload)}],
    % lager:debug("<--- ~p", [Pk]),
    <<Token:16>> = crypto:strong_rand_bytes(2),
    {ok, Timer} = timer:send_after(30000, {no_ack, Token}),
    DStamp = erlang:monotonic_time(milli_seconds),
    % PULL RESP
    ok = gen_udp:send(Socket, Host, Port, <<Version, Token:16, 3, (jsx:encode(Pk))/binary>>),
    {noreply, State#state{tokens=maps:put(Token, {Timer, DevAddr, UStamp, DStamp}, Tokens)}}.

% PUSH DATA
handle_info({udp, Socket, Host, Port, <<Version, Token:16, 0, MAC:8/binary, Data/binary>>}, #state{sock=Socket}=State) ->
    case catch jsx:decode(Data, [return_maps, {labels, atom}]) of
        Data2 when is_map(Data2) ->
            % lager:debug("---> ~p", [Data2]),
            lists:foreach(
                fun ({rxpk, Pk}) -> rxpk(MAC, Pk);
                    ({stat, Pk}) -> status(MAC, Pk);
                    % ignore non-standard ideetron/lorank extensions
                    % https://github.com/Ideetron/packet_forwarder/blob/master/poly_pkt_fwd/src/poly_pkt_fwd.c#L2085
                    ({time, _Time}) -> ok;
                    (Else) ->
                        lager:warning("Unknown element in JSON: ~p", [Else])
                end,
                maps:to_list(Data2));
        _Else ->
            lager:error("Ignored PUSH_DATA: JSON syntax error: ~s", [Data])
    end,
    % PUSH ACK
    ok = gen_udp:send(Socket, Host, Port, <<Version, Token:16, 1>>),
    {noreply, State};

% PULL DATA
handle_info({udp, Socket, Host, Port, <<Version, Token:16, 2, MAC:8/binary>>}, #state{sock=Socket}=State) ->
    lorawan_gw_router:register(MAC, {global, ?MODULE}, {Host, Port, Version}),
    lorawan_gw_router:status(MAC, undefined),
    % PULL ACK
    ok = gen_udp:send(Socket, Host, Port, <<Version, Token:16, 4>>),
    {noreply, State};

% TX ACK
handle_info({udp, Socket, _Host, _Port, <<_Version, Token:16, 5, MAC:8/binary, Data/binary>>},
        #state{sock=Socket, tokens=Tokens}=State) ->
    {Opaque, Tokens2} =
        case maps:take(Token, Tokens) of
            {{Timer, Opq, UStamp, DStamp}, Tkns} ->
                AStamp = erlang:monotonic_time(milli_seconds),
                {ok, cancel} = timer:cancel(Timer),
                {atomic, ok} = mnesia:transaction(
                    fun() ->
                        [G] = mnesia:read(gateways, MAC, write),
                        SDelay =
                            case UStamp of
                                undefined -> undefined;
                                Num -> DStamp-Num
                            end,
                        mnesia:write(gateways, store_delay(G, {calendar:universal_time(), SDelay, AStamp-DStamp}), write)
                    end),
                {Opq, Tkns};
            error ->
                {undefined, Tokens}
        end,
    case Data of
        <<>> ->
            % no error occured
            ok;
        _Else ->
            case catch jsx:decode(Data, [return_maps, {labels, atom}]) of
                Data2 when is_map(Data2) ->
                    Ack = maps:get(txpk_ack, Data2),
                    case maps:get(error, Ack, undefined) of
                        undefined -> ok;
                        <<"NONE">> -> ok;
                        Error ->
                            lorawan_gw_router:downlink_error(MAC, Opaque, Error)
                    end;
                Else ->
                    lager:error("Ignored PUSH_DATA: JSON syntax error: ~w", [Else])
            end
    end,
    {noreply, State#state{tokens=Tokens2}};

% something strange
handle_info({udp, _Socket, _Host, _Port, Msg}, State) ->
    lager:debug("Weird data ~w", [Msg]),
    {noreply, State};

handle_info({no_ack, Token}, #state{tokens=Tokens}=State) ->
    case maps:take(Token, Tokens) of
        {_, Tokens2} ->
            {noreply, State#state{tokens=Tokens2}};
        error ->
            {noreply, State}
    end.

terminate(Reason, _State) ->
    % record graceful shutdown in the log
    lager:info("packet_forwarder interface terminated: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


status(MAC, Pk) ->
    lorawan_gw_router:status(MAC, ?to_record(stat, Pk)).


rxpk(MAC, PkList) ->
    Stamp = erlang:monotonic_time(milli_seconds),
    lorawan_gw_router:uplinks(
        lists:map(
            fun(Pk) ->
                {RxQ, Data} = parse_rxpk(Pk),
                {#request{tmst=Stamp}, MAC, RxQ#rxq{srvtmst=Stamp}, Data}
            end, PkList)).

parse_rxpk(Pk) ->
    Data = base64:decode(maps:get(data, Pk)),
    % the modu field is ignored
    % we assume "LORA" datr is a binary string and "FSK" datr is an integer
    RxQ = list_to_tuple([rxq|[get_rxpk_field(X, Pk) || X <- record_info(fields, rxq)]]),
    {RxQ, Data}.

get_rxpk_field(time, List) ->
    case maps:get(time, List, undefined) of
        undefined -> undefined;
        Value -> iso8601:parse_exact(Value)
    end;
get_rxpk_field(Field, List) ->
    maps:get(Field, List, undefined).


build_txpk(TxQ, RFch, Data) ->
    Modu =
        case TxQ#txq.datr of
            Bin when is_binary(Bin) -> <<"LORA">>;
            Num when is_integer(Num) -> <<"FSK">>
        end,
    lists:foldl(
        fun ({_, undefined}, Acc) ->
                Acc;
            ({tmst, Time}, Acc) ->
                [{imme, false}, {tmst, Time} | Acc];
            ({time, immediately}, Acc) ->
                [{imme, true} | Acc];
            ({time, Time}, Acc) ->
                [{imme, false}, {time, iso8601:format(Time)} | Acc];
            ({region, _}, Acc) ->
                Acc; % internal parameter
            (Elem, Acc) -> [Elem | Acc]
        end,
        [{modu, Modu}, {rfch, RFch}, {ipol, true}, {size, byte_size(Data)}, {data, base64:encode(Data)}],
        lists:zip(record_info(fields, txq), tl(tuple_to_list(TxQ)))
    ).

store_delay(#gateway{delays=undefined}=Gateway, Delay) ->
    Gateway#gateway{delays=[Delay]};
store_delay(#gateway{delays=Past}=Gateway, Delay) ->
    Gateway#gateway{delays=lists:sublist([Delay | Past], 50)}.

% end of file
