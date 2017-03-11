%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(test_mote).

-export([push_and_pull/4, semtech_payload/1]).

-import(lorawan_mac, [reverse/1, cipher/5, b0/4]).

push_and_pull(DevCfg, FCnt, FPort, FData) ->
    Req = encode_frame(2#010, DevCfg, FCnt, 1, 1, 1, <<>>, FPort, FData),
    case test_forwarder:push_and_pull(<<0,0,0,0,0,0,0,0>>, test_forwarder:rxpk(base64:encode(Req))) of
        {ok, Resp64} ->
            process_frame(DevCfg, base64:decode(Resp64));
        Error ->
            Error
    end.

semtech_payload(LED) ->
    <<LED, 0:16, 0:16, 0:16, 0, 0:24, 0:24, 0:16>>.

encode_frame(MType, DevCfg, FCnt, ADR, ADRACKReq, ACK, FOpts, FPort, FData) ->
    {DevAddr, NwkSKey, AppSKey} = get_config(DevCfg),
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

process_frame(DevCfg, PHYPayload) ->
    Size = byte_size(PHYPayload)-4,
    <<Msg:Size/binary, MIC:4/binary>> = PHYPayload,
    <<MType:3, _:5, _/binary>> = Msg,
    process_frame0(DevCfg, MType, Msg, MIC).

process_frame0(DevCfg, MType, Msg, MIC) ->
    {DevAddr, NwkSKey, AppSKey} = get_config(DevCfg),
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
                    {ok, FPort, reverse(Data)};
                0 ->
                    {error, double_fopts};
                _N ->
                    Data = cipher(FRMPayload, AppSKey, MType band 1, DevAddr, FCnt),
                    {ok, FPort, reverse(Data)}
            end;
        _MIC2 ->
            {error, bad_mic}
    end.

get_config({DevAddr, NwkSKey, AppSKey}) ->
    {lorawan_mac:hex_to_binary(DevAddr),
        lorawan_mac:hex_to_binary(NwkSKey), lorawan_mac:hex_to_binary(AppSKey)}.

% end of file
