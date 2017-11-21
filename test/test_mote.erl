%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(test_mote).
-behaviour(gen_server).

-export([start_link/2, stop/1, push_and_pull/4, semtech_payload/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {devaddr, nwkskey, appskey, gateway, client, send_status}).
-import(lorawan_mac, [reverse/1, cipher/5, b0/4]).

-include_lib("eunit/include/eunit.hrl").

start_link(DevCfg, Gateway) ->
    gen_server:start_link(?MODULE, [DevCfg, Gateway], []).

stop(Mote) ->
    gen_server:stop(Mote).

push_and_pull(Mote, FCnt, FPort, FData) ->
    Mote ! {self(), FCnt, FPort, FData},
    receive
        Response -> Response
        after 2000 -> {error, timeout}
    end.

init([DevCfg, Gateway]) ->
    {DevAddr, NwkSKey, AppSKey} = get_config(DevCfg),
    {ok, #state{devaddr=DevAddr, nwkskey=NwkSKey, appskey=AppSKey, gateway=Gateway,
        client=undefined, send_status=false}}.

get_config({DevAddr, NwkSKey, AppSKey}) ->
    {DevAddr, lorawan_mac:hex_to_binary(NwkSKey), lorawan_mac:hex_to_binary(AppSKey)}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({From, FCnt, FPort, FData}, #state{gateway=Gateway}=State) ->
    Req = encode_frame(2#010, FCnt, 0, 0, 0, <<>>, FPort, FData, State),
    Gateway ! {uplink, self(), test_forwarder:rxpk(base64:encode(Req))},
    {noreply, State#state{client=From}};

handle_info({ok, Resp64}, #state{client=Client}=State) ->
    {Reply, State2} = process_frame(base64:decode(Resp64), State),
    Client ! Reply,
    {noreply, State2}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


semtech_payload(LED) ->
    <<LED, 0:16, 0:16, 0:16, 0, 0:24, 0:24, 0:16>>.

encode_frame(MType, FCnt, ADR, ADRACKReq, ACK, FOpts, FPort, FData,
        #state{devaddr=DevAddr, nwkskey=NwkSKey, appskey=AppSKey}) ->
    FHDR = <<(reverse(DevAddr)):4/binary, ADR:1, ADRACKReq:1, ACK:1, 0:1, (byte_size(FOpts)):4,
        FCnt:16/little-unsigned-integer, FOpts/binary>>,
    MACPayload = case FPort of
        undefined ->
            <<FHDR/binary>>;
        0 ->
            FRMPayload = cipher(FData, NwkSKey, MType band 1, DevAddr, FCnt),
            <<FHDR/binary, 0:8, (reverse(FRMPayload))/binary>>;
        Num when Num > 0 ->
            FRMPayload = cipher(FData, AppSKey, MType band 1, DevAddr, FCnt),
            <<FHDR/binary, FPort:8, (reverse(FRMPayload))/binary>>
    end,
    Msg = <<MType:3, 0:3, 0:2, MACPayload/binary>>,
    MIC = aes_cmac:aes_cmac(NwkSKey, <<(b0(MType band 1, DevAddr, FCnt, byte_size(Msg)))/binary, Msg/binary>>, 4),
    <<Msg/binary, MIC/binary>>.

process_frame(PHYPayload, State) ->
    Size = byte_size(PHYPayload)-4,
    <<Msg:Size/binary, MIC:4/binary>> = PHYPayload,
    <<MType:3, _:5, _/binary>> = Msg,
    process_frame0(MType, Msg, MIC, State).

process_frame0(MType, Msg, MIC, #state{devaddr=DevAddr, nwkskey=NwkSKey, appskey=AppSKey}=State) ->
    <<_, MACPayload/binary>> = Msg,
    <<DevAddr0:4/binary, ADR:1, _RFU:1, ACK:1, Pending:1, FOptsLen:4,
        FCnt:16/little-unsigned-integer, FOpts:FOptsLen/binary, Body/binary>> = MACPayload,
    {FPort, FRMPayload} = case Body of
        <<>> -> {undefined, <<>>};
        <<Port:8, Payload/binary>> -> {Port, Payload}
    end,
    DevAddr = reverse(DevAddr0),
    case aes_cmac:aes_cmac(NwkSKey, <<(b0(MType band 1, DevAddr, FCnt, byte_size(Msg)))/binary, Msg/binary>>, 4) of
        MIC ->
            case FPort of
                0 when FOptsLen == 0 ->
                    Data = cipher(FRMPayload, NwkSKey, MType band 1, DevAddr, FCnt),
                    {{ok, FPort, reverse(Data)}, State};
                0 ->
                    {{error, double_fopts}, State};
                _N ->
                    State2 =
                        lists:foldl(
                            fun (dev_status_req, S) -> S#state{send_status=true}
                            end,
                            State, parse_fopts(FOpts)),
                    Data = cipher(FRMPayload, AppSKey, MType band 1, DevAddr, FCnt),
                    {{ok, FPort, reverse(Data)}, State2}
            end;
        _MIC2 ->
            {{error, bad_mic}, State}
    end.

parse_fopts(<<16#02, Margin, GwCnt, Rest/binary>>) ->
    [{link_check_ans, Margin, GwCnt} | parse_fopts(Rest)];
parse_fopts(<<16#03, DataRate:4, TXPower:4, ChMask:16/little-unsigned-integer, _RFU:1, ChMaskCntl:3, NbRep:4, Rest/binary>>) ->
    [{link_adr_req, DataRate, TXPower, ChMask, ChMaskCntl, NbRep} | parse_fopts(Rest)];
parse_fopts(<<16#04, MaxDCycle, Rest/binary>>) ->
    [{duty_cycle_req, MaxDCycle} | parse_fopts(Rest)];
parse_fopts(<<16#05, _RFU:1, RX1DROffset:3, RX2DataRate:4, Frequency:24/little-unsigned-integer, Rest/binary>>) ->
    [{rx_param_setup_req, RX1DROffset, RX2DataRate, Frequency} | parse_fopts(Rest)];
parse_fopts(<<16#06, Rest/binary>>) ->
    [dev_status_req | parse_fopts(Rest)];
parse_fopts(<<16#07, ChIndex, Freq:24/little-unsigned-integer, MaxDR:4, MinDR:4, Rest/binary>>) ->
    [{new_channel_req, ChIndex, Freq, MaxDR, MinDR} | parse_fopts(Rest)];
parse_fopts(<<16#08, _RFU:4, Delay:4, Rest/binary>>) ->
    [{rx_timing_setup_req, Delay} | parse_fopts(Rest)];
parse_fopts(<<>>) ->
    [];
parse_fopts(Unknown) ->
    lager:warning("Unknown command ~w", [Unknown]),
    [].

encode_fopts([link_check_req | Rest]) ->
    <<16#02, (encode_fopts(Rest))/binary>>;
encode_fopts([{link_adr_ans, PowerACK, DataRateACK, ChannelMaskACK} | Rest]) ->
    <<16#03, 0:5, PowerACK:1, DataRateACK:1, ChannelMaskACK:1, (encode_fopts(Rest))/binary>>;
encode_fopts([duty_cycle_ans | Rest]) ->
    <<16#04, (encode_fopts(Rest))/binary>>;
encode_fopts([{rx_param_setup_ans, RX1DROffsetACK, RX2DataRateACK, ChannelACK} | Rest]) ->
    <<16#05, 0:5, RX1DROffsetACK:1, RX2DataRateACK:1, ChannelACK:1, (encode_fopts(Rest))/binary>>;
encode_fopts([{dev_status_ans, Battery, Margin} | Rest]) ->
    <<16#06, Battery:8, 0:2, Margin:6, (encode_fopts(Rest))/binary>>;
encode_fopts([{new_channel_ans, DataRateRangeOK, ChannelFreqOK} | Rest]) ->
    <<16#07, 0:6, DataRateRangeOK:1, ChannelFreqOK:1, (encode_fopts(Rest))/binary>>;
encode_fopts([rx_timing_setup_ans | Rest]) ->
    <<16#08, (encode_fopts(Rest))/binary>>;
encode_fopts([]) ->
    <<>>.

% end of file
