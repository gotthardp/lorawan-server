%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
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

-include("lorawan_db.hrl").
-include("lorawan.hrl").

-record(state, {sock, tokens}).

start_link(PktFwdOpts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PktFwdOpts], []).

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

handle_cast(_Request, State) ->
    {stop, State}.

% PUSH DATA
handle_info({udp, Socket, Host, Port, <<Version, Token:16, 0, MAC:8/binary, Data/binary>>}, #state{sock=Socket}=State) ->
    % PUSH ACK
    ok = gen_udp:send(Socket, Host, Port, <<Version, Token:16, 1>>),
    % process packets after ack
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
        _ ->
            lager:error("Ignored PUSH_DATA from ~s: JSON syntax error: ~s", [lorawan_utils:binary_to_hex(MAC), Data])
    end,
    {noreply, State};

% PULL DATA
handle_info({udp, Socket, Host, Port, <<Version, Token:16, 2, MAC:8/binary>>}, #state{sock=Socket}=State) ->
    % PULL ACK
    ok = gen_udp:send(Socket, Host, Port, <<Version, Token:16, 4>>),
    lorawan_gw_router:alive(MAC, ?MODULE, {Host, Port, Version}),
    {noreply, State};

% TX ACK
handle_info({udp, Socket, _Host, _Port, <<_Version, Token:16, 5, MAC:8/binary, Data/binary>>},
        #state{sock=Socket, tokens=Tokens}=State) ->
    {DevAddr, Tokens2} =
        case maps:take(Token, Tokens) of
            {{Timer, AState, DStamp}, Tkns} ->
                AStamp = erlang:monotonic_time(milli_seconds),
                {ok, cancel} = timer:cancel(Timer),
                lorawan_gw_router:network_delay(MAC, AStamp-DStamp),
                {AState, Tkns};
            error ->
                {undefined, Tokens}
        end,
    case trim_json(Data) of
        <<>> ->
            % no error occured
            ok;
        _ ->
            case catch jsx:decode(Data, [return_maps, {labels, atom}]) of
                Data2 when is_map(Data2) ->
                    Ack = maps:get(txpk_ack, Data2),
                    case maps:get(error, Ack, undefined) of
                        undefined -> ok;
                        <<"NONE">> -> ok;
                        Error ->
                            lorawan_gw_router:downlink_error(MAC, DevAddr,
                                list_to_binary(string:to_lower(binary_to_list(Error))))
                    end;
                _ ->
                    lager:error("Ignored TX_ACK from ~s: JSON syntax error: ~s", [lorawan_utils:binary_to_hex(MAC), Data])
            end
    end,
    {noreply, State#state{tokens=Tokens2}};

% something strange
handle_info({udp, _Socket, Host, Port, Msg}, State) ->
    lager:warning("Weird data from ~s:~p: ~w", [inet:ntoa(Host), Port, Msg]),
    {noreply, State};

handle_info({send, {Host, Port, Version}, GWState, DevAddr, TxQ, RFCh, PHYPayload},
        #state{sock=Socket, tokens=Tokens}=State) ->
    Pk = [{txpk, build_txpk(TxQ, GWState, RFCh, PHYPayload)}],
    % lager:debug("<--- ~p", [Pk]),
    <<Token:16>> = crypto:strong_rand_bytes(2),
    {ok, Timer} = timer:send_after(30000, {no_ack, Token}),
    DStamp = erlang:monotonic_time(milli_seconds),
    % PULL RESP
    ok = gen_udp:send(Socket, Host, Port, <<Version, Token:16, 3, (jsx:encode(Pk))/binary>>),
    {noreply, State#state{tokens=maps:put(Token, {Timer, DevAddr, DStamp}, Tokens)}};

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
    lorawan_gw_router:report(MAC, ?to_record(stat, Pk)).


rxpk(MAC, PkList) ->
    lorawan_gw_router:uplinks(
        lists:map(
            fun(Pk) ->
                {RxQ, GWState, PHYPayload} = parse_rxpk(Pk),
                {{MAC, RxQ, GWState}, PHYPayload}
            end, PkList)).

parse_rxpk(#{tmst:=TmSt, freq:=Freq, datr:=DatR, codr:=CodR, data:=Data}=Pk) ->
    % search for a rsig entry with the best RSSI
    Ant =
        lists:foldl(
            fun
                (A1, undefined) ->
                    A1;
                (#{rssic := RSSI1}=A1, #{rssic := RSSI2}) when RSSI1 > RSSI2 ->
                    A1;
                (_Else, A2) ->
                    A2
            end,
            undefined, get_or_default(rsig, Pk, [])),
    % the modu field is ignored
    % we assume "LORA" datr is a binary string and "FSK" datr is an integer
    {#rxq{
            freq=Freq,
            datr=DatR,
            codr=CodR,
            time=
                case get_or_undefined(time, Pk) of
                    undefined -> undefined;
                    Value -> iso8601:parse_exact(Value)
                end,
            tmms=get_or_undefined(tmms, Pk),
            rssi=
                case {Pk, Ant} of
                    {#{rssi := RSSI}, _} when is_number(RSSI) -> RSSI;
                    {_, #{rssic := RSSI}} when is_number(RSSI) -> RSSI;
                    _ -> undefined
                end,
            lsnr=
                case {Pk, Ant} of
                    {#{lsnr := SNR}, _} when is_number(SNR) -> SNR;
                    {_, #{lsnr := SNR}} when is_number(SNR) -> SNR;
                    _ -> undefined
                end
        },
        #{tmst => TmSt},
        base64:decode(Data)}.

% the JSON may contain "null" values that shall be converted to undefined/default
get_or_undefined(Key, Map) ->
    get_or_default(Key, Map, undefined).
get_or_default(Key, Map, Default) ->
    case maps:get(Key, Map, undefined) of
        undefined -> Default;
        null -> Default;
        Else -> Else
    end.

build_txpk(#txq{freq=Freq, datr=DatR, codr=CodR, time=Time, powe=Power}, GWState, RFch, Data) ->
    case Time of
        % class A
        Num when is_number(Num) ->
            #{tmst:=TmSt} = GWState,
            [{imme, false}, {tmst, TmSt+Time*1000000}];
        % class C
        immediately ->
            % note that GWState is undefined in this case
            [{imme, true}];
        Stamp ->
            [{imme, false}, {time, iso8601:format(Stamp)}]
    end ++ [
    {freq, Freq},
    {rfch, RFch},
    {powe, Power},
    {modu,
        if
            is_binary(DatR) -> <<"LORA">>;
            is_integer(DatR) -> <<"FSK">>
        end},
    {datr, DatR},
    {codr, CodR},
    {ipol, true},
    {size, byte_size(Data)},
    {data, base64:encode(Data)}].

% some gateways send <<0>>
trim_json(<<0, Rest/binary>>) ->
    trim_json(Rest);
trim_json(<<$\s, Rest/binary>>) ->
    trim_json(Rest);
trim_json(<<$\t, Rest/binary>>) ->
    trim_json(Rest);
trim_json(Rest) ->
    Rest.

-include_lib("eunit/include/eunit.hrl").

trim_test_() ->
    [?_assertEqual(<<>>, trim_json(<<>>)),
    ?_assertEqual(<<>>, trim_json(<<0>>)),
    ?_assertEqual(<<>>, trim_json(<<"  \t\t">>)),
    ?_assertEqual(<<"{\"one\": 1}">>, trim_json(<<"  {\"one\": 1}">>))].

% end of file
