%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
% LoRaWAN 1.0.1 compliant MAC implementation
% Supports Class A devices
%
-module(lorawan_mac).

-export([process_frame/4, process_status/2]).
-export([binary_to_hex/1, hex_to_binary/1]).

-define(MAX_FCNT_GAP, 16384).
-include("lorawan.hrl").

process_frame(MAC, RxQ, RF, PHYPayload) ->
    Size = byte_size(PHYPayload)-4,
    <<Msg:Size/binary, MIC:4/binary>> = PHYPayload,
    <<MType:3, _:5, _/binary>> = Msg,
    case mnesia:dirty_read(gateways, MAC) of
        [] ->
            {error, {unknown_mac, MAC}};
        [G] ->
            process_frame1(G#gateway.netid, MAC, RxQ, RF, MType, Msg, MIC)
    end.

process_status(MAC, S) ->
    case mnesia:dirty_read(gateways, MAC) of
        [] ->
            {error, {unknown_mac, MAC}};
        [G] ->
            G2 = if
                % store gateway GPS position
                is_number(S#stat.lati), is_number(S#stat.long), is_number(S#stat.alti),
                S#stat.lati /= 0, S#stat.long /= 0, S#stat.alti /= 0 ->
                    G#gateway{ gpspos={S#stat.lati, S#stat.long}, gpsalt=S#stat.alti };
                % position not received
                true -> G
            end,
            mnesia:dirty_write(gateways, G2),
            ok
    end.

process_frame1(NetID, _MAC, RxQ, RF, 0, Msg, MIC) ->
    <<_, MACPayload/binary>> = Msg,
    <<AppEUI0:8/binary, DevEUI0:8/binary, DevNonce:2/binary>> = MACPayload,
    {AppEUI, DevEUI} = {reverse(AppEUI0), reverse(DevEUI0)},

    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            {error, {unknown_deveui, DevEUI}};
        [D] ->
            case aes_cmac:aes_cmac(D#device.appkey, Msg, 4) of
                MIC ->
                    handle_join(NetID, RxQ, RF, AppEUI, DevEUI, DevNonce, D#device.appkey);
                _MIC2 ->
                    {error, bad_mic}
            end
    end;
process_frame1(_NetID, MAC, RxQ, RF, MType, Msg, MIC) ->
    <<_, MACPayload/binary>> = Msg,
    <<DevAddr0:4/binary, _ADR:1, _ADRACKReq:1, _ACK:1, _FPending:1, FOptsLen:4,
        FCnt:16/little-unsigned-integer, _FOpts:FOptsLen/binary, FPort:8, FRMPayload/binary>> = MACPayload,
    DevAddr = reverse(DevAddr0),

    case check_link(DevAddr, FCnt) of
        {ok, L} ->
            case aes_cmac:aes_cmac(L#link.nwkskey, <<(b0(MType band 1, DevAddr, FCnt, byte_size(Msg)))/binary, Msg/binary>>, 4) of
                MIC ->
                    Data = cipher(FRMPayload, L#link.appskey, MType band 1, DevAddr, FCnt),
                    store_rxpk(MAC, RxQ, RF, DevAddr, FCnt, reverse(Data)),
                    handle_rxpk(RxQ, MType, DevAddr, L#link.app, L#link.appid, FPort, reverse(Data));
                _MIC2 ->
                    {error, bad_mic}
            end;
        {error, Error} ->
            {error, Error}
    end.


handle_join(NetID, RxQ, RF, AppEUI, DevEUI, DevNonce, AppKey) ->
    AppNonce = crypto:strong_rand_bytes(3),
    NwkSKey = crypto:block_encrypt(aes_ecb, AppKey,
        padded(16, <<16#01, AppNonce/binary, NetID/binary, DevNonce/binary>>)),
    AppSKey = crypto:block_encrypt(aes_ecb, AppKey,
        padded(16, <<16#02, AppNonce/binary, NetID/binary, DevNonce/binary>>)),

    {atomic, DevAddr} = mnesia:transaction(fun() ->
        [D] = mnesia:read(devices, DevEUI, write),
        NewAddr = if
            D#device.link == undefined;
            byte_size(D#device.link) < 4 ->
                Created = create_devaddr(NetID),
                ok = mnesia:write(devices, D#device{link=Created}, write),
                Created;
            true -> D#device.link
        end,

        lager:info("JOIN REQUEST ~w ~w -> ~w",[AppEUI, DevEUI, NewAddr]),
        ok = mnesia:write(links, #link{devaddr=NewAddr, app=D#device.app, appid=D#device.appid, nwkskey=NwkSKey, appskey=AppSKey, fcntup=0, fcntdown=0}, write),
        NewAddr
    end),

    % transmitting after join accept delay 1
    Time = RxQ#rxq.tmst + 5000000,
    txaccept(Time, RF, AppKey, AppNonce, NetID, DevAddr).

create_devaddr(NetID) ->
    <<_:17, NwkID:7>> = NetID,
    %% FIXME: verify uniqueness
    <<NwkID:7, 0:1, (crypto:strong_rand_bytes(3))/binary>>.

txaccept(Time, RF, AppKey, AppNonce, NetID, DevAddr) ->
    MHDR = <<1:3, 0:3, 0:2>>,
    MACPayload = <<AppNonce/binary, NetID/binary, (reverse(DevAddr))/binary, 0:1, 0:3, 3:4, 1>>,
    MIC = aes_cmac:aes_cmac(AppKey, <<MHDR/binary, MACPayload/binary>>, 4),

    % yes, decrypt; see LoRaWAN specification, Section 6.2.5
    PHYPayload = crypto:block_decrypt(aes_ecb, AppKey, padded(16, <<MACPayload/binary, MIC/binary>>)),
    {send, Time, RF, <<MHDR/binary, PHYPayload/binary>>}.


check_link(DevAddr, FCnt) ->
    case mnesia:dirty_read(links, DevAddr) of
        [] ->
            {error, {unknown_devaddr, DevAddr}};
        [L] ->
            case fcnt_gap(L#link.fcntup, FCnt) of
                N when N < ?MAX_FCNT_GAP ->
                    % L#link.fcntup is 32b, but the received FCnt may be 16b only
                    LastFCnt = (L#link.fcntup + N) band 16#FFFFFFFF,
                    mnesia:dirty_write(links, L#link{fcntup = LastFCnt}),
                    {ok, L};
                BigN ->
                    lager:error("~w has a large FCnt gap: last ~b, current ~b", [DevAddr, L#link.fcntup, FCnt]),
                    {error, {fcnt_gap_too_large, BigN}}
            end
    end.

fcnt_gap(A, B) ->
    A16 = A band 16#FFFF,
    if
        A16 > B -> 16#FFFF - A16 + B;
        true  -> B - A16
    end.

store_rxpk(MAC, RxQ, RF, DevAddr, FCnt, Data) ->
    % TODO: store the data is a proper time-series database
    ok.

handle_rxpk(RxQ, 2, DevAddr, App, AppID, Port, Data) ->
    case lorawan_application:handle(DevAddr, App, AppID, Port, Data) of
        {send, PortOut, DataOut} ->
            % transmitting for the RX2 window
            Time = RxQ#rxq.tmst + 2000000,
            txpk(Time, #rflora{freq=869.525, datr= <<"SF9BW125">>, codr= <<"4/5">>}, DevAddr, PortOut, DataOut);
        ok -> ok;
        {error, Error} -> {error, Error}
    end.

txpk(Time, RF, DevAddr, FPort, Data) ->
    {atomic, L} = mnesia:transaction(fun() ->
        [D] = mnesia:read(links, DevAddr, write),
        FCnt =  (D#link.fcntdown + 1) band 16#FFFFFFFF,
        NewD = D#link{fcntdown=FCnt},
        mnesia:write(links, NewD, write),
        NewD
    end),

    FRMPayload = cipher(Data, L#link.appskey, 1, DevAddr, L#link.fcntdown),
    MACPayload = <<(reverse(DevAddr)):4/binary, 0, (L#link.fcntdown):16/little-unsigned-integer, FPort:8, (reverse(FRMPayload))/binary>>,
    Msg = <<3:3, 0:3, 0:2, MACPayload/binary>>,
    MIC = aes_cmac:aes_cmac(L#link.nwkskey, <<(b0(1, DevAddr, L#link.fcntdown, byte_size(Msg)))/binary, Msg/binary>>, 4),
    PHYPayload = <<Msg/binary, MIC/binary>>,
    {send, Time, RF, PHYPayload}.


cipher(Bin, Key, Dir, DevAddr, FCnt) ->
    cipher(Bin, Key, Dir, DevAddr, FCnt, 1, <<>>).

cipher(<<Block:16/binary, Rest/binary>>, Key, Dir, DevAddr, FCnt, I, Acc) ->
    Si = crypto:block_encrypt(aes_ecb, Key, ai(Dir, DevAddr, FCnt, I)),
    cipher(Rest, Key, Dir, DevAddr, FCnt, I+1, <<(binxor(Block, Si, <<>>))/binary, Acc/binary>>);
cipher(<<>>, _Key, _Dir, _DevAddr, _FCnt, _I, Acc) -> Acc;
cipher(<<LastBlock/binary>>, Key, Dir, DevAddr, FCnt, I, Acc) ->
    Si = crypto:block_encrypt(aes_ecb, Key, ai(Dir, DevAddr, FCnt, I)),
    <<(binxor(LastBlock, binary:part(Si, 0, byte_size(LastBlock)), <<>>))/binary, Acc/binary>>.

ai(Dir, DevAddr, FCnt, I) ->
    <<16#01, 0,0,0,0, Dir, (reverse(DevAddr)):4/binary, FCnt:32/little-unsigned-integer, 0, I>>.

b0(Dir, DevAddr, FCnt, Len) ->
    <<16#49, 0,0,0,0, Dir, (reverse(DevAddr)):4/binary, FCnt:32/little-unsigned-integer, 0, Len>>.

binxor(<<>>, <<>>, Acc) -> Acc;
binxor(<<A, RestA/binary>>, <<B, RestB/binary>>, Acc) ->
    binxor(RestA, RestB, <<(A bxor B), Acc/binary>>).

reverse(Bin) -> reverse(Bin, <<>>).
reverse(<<>>, Acc) -> Acc;
reverse(<<H:1/binary, Rest/binary>>, Acc) ->
    reverse(Rest, <<H/binary, Acc/binary>>).

padded(Bytes, Msg) ->
    case bit_size(Msg) rem (8*Bytes) of
        0 -> Msg;
        N -> <<Msg/bitstring, 0:(8*Bytes-N)>>
    end.

% stackoverflow.com/questions/3768197/erlang-ioformatting-a-binary-to-hex
% a little magic from http://stackoverflow.com/users/2760050/himangshuj
binary_to_hex(Id) ->
    << <<Y>> || <<X:4>> <= Id, Y <- integer_to_list(X,16)>>.

hex_to_binary(Id) ->
    <<<<Z>> || <<X:8,Y:8>> <= Id,Z <- [binary_to_integer(<<X,Y>>,16)]>>.

% end of file
