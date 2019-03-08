%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_mac).

-export([ingest_frame/2, handle_accept/4, load_profile/1, encode_unicast/4, encode_multicast/2]).
% for unit testing
-export([cipher/5, b0/4]).
-import(lorawan_utils, [binary_to_hex/1, hex_to_binary/1, reverse/1]).

-define(MAX_FCNT_GAP, 16384).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

% TODO complete type specification
-spec ingest_frame(binary(), binary()) -> any().
ingest_frame(MAC, <<MType:3, _:3, 0:2, _/binary>> = PHYPayload) when byte_size(PHYPayload) > 4 ->
    Size = byte_size(PHYPayload)-4,
    <<Msg:Size/binary, MIC:4/binary>> = PHYPayload,
    {atomic, Res} =
        mnesia:transaction(
            fun() ->
                ingest_frame0(MAC, MType, Msg, MIC)
            end),
    Res;
ingest_frame(MAC, PHYPayload) ->
    lager:warning("gateway ~s received unknown frame protocol: ~w", [binary_to_hex(MAC), PHYPayload]),
    ignore.

ingest_frame0(MAC, 2#000, <<_, AppEUI0:8/binary, DevEUI0:8/binary,
        DevNonce:2/binary>> = Msg, MIC) ->
    {AppEUI, DevEUI} = {reverse(AppEUI0), reverse(DevEUI0)},
    case mnesia:read(device, DevEUI, read) of
        [] ->
            {error, {device, DevEUI}, unknown_deveui, aggregated};
        [D] when D#device.appeui /= undefined, D#device.appeui /= AppEUI ->
            {error, {device, DevEUI}, {bad_appeui, binary_to_hex(AppEUI)}, aggregated};
        [D] ->
            case aes_cmac:aes_cmac(D#device.appkey, Msg, 4) of
                MIC ->
                    verify_join(MAC, D, DevNonce);
                _MIC2 ->
                    {error, {device, DevEUI}, bad_mic}
            end
    end;
ingest_frame0(MAC, MType, <<_, DevAddr0:4/binary, ADR:1, ADRACKReq:1, ACK:1, _RFU:1,
        FOptsLen:4, FCnt:16/little-unsigned-integer, FOpts:FOptsLen/binary,
        Body/binary>> = Msg, MIC)
        when MType == 2#010; MType == 2#011; MType == 2#100; MType == 2#101 ->
    <<Confirm:1, _:2>> = <<MType:3>>,
    DevAddr = reverse(DevAddr0),
    {Port, FRMPayload} = case Body of
        <<>> -> {undefined, <<>>};
        <<FPort:8, FPayload/binary>> -> {FPort, FPayload}
    end,
    ingest_data_frame(MAC, MType, Msg, FOpts, FRMPayload, MIC,
        #frame{conf=Confirm, devaddr=DevAddr, adr=ADR, adr_ack_req=ADRACKReq, ack=ACK, fcnt=FCnt, port=Port});
ingest_frame0(MAC, MType, Msg, _MIC) ->
    lager:warning("gateway ~s received bad frame (mtype ~.2B): ~p", [binary_to_hex(MAC), MType, Msg]),
    ignore.

ingest_data_frame(_MAC, MType, Msg, FOpts, FRMPayload, MIC,
        #frame{devaddr=DevAddr, fcnt=FCnt, port=Port}=Frame)
        when MType == 2#010; MType == 2#100 ->
    case accept_node_frame(DevAddr, FCnt) of
        {ok, Fresh, {Network, Profile, Node}} ->
            case aes_cmac:aes_cmac(Node#node.nwkskey,
                    <<(b0(MType band 1, DevAddr, Node#node.fcntup, byte_size(Msg)))/binary, Msg/binary>>, 4) of
                MIC ->
                    ok = lorawan_admin:write(
                            ensure_used_fields(Network, Node)),
                    case Port of
                        0 when byte_size(FOpts) == 0 ->
                            Data = cipher(FRMPayload, Node#node.nwkskey, MType band 1, DevAddr, Node#node.fcntup),
                            {Fresh, {Network, Profile, Node},
                                Frame#frame{fopts=reverse(Data), data= <<>>}};
                        0 ->
                            {error, {node, DevAddr}, double_fopts};
                        _N ->
                            Data = cipher(FRMPayload, Node#node.appskey, MType band 1, DevAddr, Node#node.fcntup),
                            {Fresh, {Network, Profile, Node},
                                Frame#frame{fopts=FOpts, data=reverse(Data)}}
                    end;
                _MIC2 ->
                    {error, {node, DevAddr}, bad_mic}
            end;
        {error, ignored_node} ->
            {ignore, Frame};
        {error, Error, Args} ->
            lorawan_utils:throw_error({node, DevAddr}, Error, Args),
            {ignore, Frame}
    end;
ingest_data_frame(MAC, MType, _Msg, _FOpts, _FRMPayload, _MIC, #frame{devaddr=DevAddr}) ->
    lager:warning("gateway ~s received ~s downlink frame (mtype ~.2B)", [binary_to_hex(MAC), binary_to_hex(DevAddr), MType]),
    ignore.

verify_join(MAC, #device{deveui=DevEUI, profile=ProfID}=Device, DevNonce) ->
    case mnesia:read(profile, ProfID, read) of
        [] ->
            {error, {device, DevEUI}, {unknown_profile, ProfID}, aggregated};
        [#profile{join=0}] ->
            lager:warning("gateway ~s ignored join from DevEUI ~s", [binary_to_hex(MAC), binary_to_hex(DevEUI)]),
            ignore;
        [#profile{join=Join}=Profile] ->
            case known_devnonce(DevNonce, Device) of
                true when Join == 1 ->
                    {error, {device, DevEUI}, second_join};
                _ ->
                    handle_join(MAC, Profile, Device, DevNonce)
            end
    end.

known_devnonce(_DevNonce, #device{last_joins=undefined}) ->
    false;
known_devnonce(DevNonce, #device{last_joins=Past}) ->
    {_, Nonces} = lists:unzip(Past),
    lists:member(DevNonce, Nonces).

handle_join(MAC, #profile{group=GroupName}=Profile, #device{deveui=DevEUI}=Device, DevNonce) ->
    case mnesia:read(group, GroupName, read) of
        [] ->
            {error, {device, DevEUI}, {unknown_group, GroupName}, aggregated};
        [#group{can_join=false}] ->
            lager:warning("gateway ~s ignored join from DevEUI ~s", [binary_to_hex(MAC), binary_to_hex(DevEUI)]),
            ignore;
        [#group{network=NetName, subid=SubID}] ->
            case mnesia:read(network, NetName, read) of
                [] ->
                    {error, {device, DevEUI}, {unknown_network, NetName}, aggregated};
                [#network{netid=NetID}=Network] ->
                    DevAddr = get_devaddr(Device, NetID, SubID),
                    {join, {Network, Profile, Device}, DevAddr, DevNonce}
            end
    end.

get_devaddr(#device{node=DevAddr}, _, _)
        when is_binary(DevAddr), byte_size(DevAddr) == 4 ->
    DevAddr;
get_devaddr(#device{}, NetID, SubID) ->
    create_devaddr(NetID, SubID, 3).

create_devaddr(NetID, SubID, Attempts) ->
    <<_:17, NwkID:7>> = NetID,
    DevAddr =
        case SubID of
            undefined ->
                <<NwkID:7, (rand_bitstring(25))/bitstring>>;
            Bits ->
                <<NwkID:7, Bits/bitstring, (rand_bitstring(25-bit_size(Bits)))/bitstring>>
        end,
    % assert uniqueness
    case mnesia:read(node, DevAddr, read) of
        [] ->
            DevAddr;
        [#node{}] when Attempts > 0 ->
            create_devaddr(NetID, SubID, Attempts-1)
        %% FIXME: do not crash when Attempts == 0
    end.

rand_bitstring(Num) when Num rem 8 > 0 ->
    <<Bits:Num/bitstring, _/bitstring>> = crypto:strong_rand_bytes(1 + Num div 8),
    Bits;
rand_bitstring(Num) when Num rem 8 == 0 ->
    crypto:strong_rand_bytes(Num div 8).

reset_node(DevAddr) ->
    ok = mnesia:dirty_delete(pending, DevAddr),
    % delete previously queued TX frames
    lorawan_db_guard:purge_queued(DevAddr).


accept_node_frame(DevAddr, FCnt) ->
    case is_ignored(DevAddr, mnesia:dirty_all_keys(ignored_node)) of
        false ->
            case load_node(DevAddr) of
                {ok, {Network, Profile, Node}} ->
                    case check_fcnt({Network, Profile, Node}, FCnt) of
                        {ok, Fresh, Node2} ->
                            {ok, Fresh, {Network, Profile, Node2}};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        true ->
            {error, ignored_node}
    end.

is_ignored(_DevAddr, []) ->
    false;
is_ignored(DevAddr, [Key|Rest]) ->
    [#ignored_node{devaddr=MatchAddr, mask=MatchMask}] = mnesia:dirty_read(ignored_node, Key),
    case match(DevAddr, MatchAddr, MatchMask) of
        true -> true;
        false -> is_ignored(DevAddr, Rest)
    end.

match(<<DevAddr:32>>, <<MatchAddr:32>>, undefined) ->
    DevAddr == MatchAddr;
match(<<DevAddr:32>>, <<MatchAddr:32>>, <<MatchMask:32>>) ->
    (DevAddr band MatchMask) == MatchAddr;
match(<<_DevAddr:32>>, _Else, _) ->
    false.

load_node(DevAddr) ->
    case mnesia:read(node, DevAddr, write) of
        [] ->
            case in_our_network(DevAddr) of
                true ->
                    % report errors for devices from own network only
                    {error, unknown_devaddr, aggregated};
                false ->
                    {error, ignored_node}
            end;
        [#node{profile=ProfID}=Node] ->
            case load_profile(ProfID) of
                {ok, Network, Profile} ->
                    {ok, {Network, Profile, Node}};
                Error ->
                    Error
            end
    end.

in_our_network(DevAddr) ->
    lists:any(
        fun({<<_:17, NwkID:7>>, SubID}) ->
            {MyPrefix, MyPrefixSize} =
                case SubID of
                    undefined ->
                        {NwkID, 7};
                    Bits ->
                        {<<NwkID:7, Bits/bitstring>>, 7+bit_size(Bits)}
                end,
            case DevAddr of
                <<MyPrefix:MyPrefixSize/bitstring, _/bitstring>> ->
                    true;
                _Else ->
                    false
            end
        end,
        lists:map(
            fun(#group{network=NetName, subid=SubId}) ->
                [#network{netid=NetId}] = mnesia:read(network, NetName, read),
                {NetId, SubId}
            end,
            mnesia:select(group, [{#group{_='_'}, [], ['$_']}], read))).

load_profile(ProfID) ->
    case mnesia:read(profile, ProfID, read) of
        [] ->
            {error, {unknown_profile, ProfID}, aggregated};
        [#profile{group=GroupName}=Profile] ->
            case mnesia:read(group, GroupName, read) of
                [] ->
                    {error, {unknown_group, GroupName}, aggregated};
                [#group{network=NetName}] ->
                    case mnesia:read(network, NetName, read) of
                        [] ->
                            {error, {unknown_network, NetName}, aggregated};
                        [Network] ->
                            {ok, Network, Profile}
                    end
            end
    end.

check_fcnt({Network, Profile, Node}, FCnt) ->
    {ok, MaxLost} = application:get_env(lorawan_server, max_lost_after_reset),
    if
        Node#node.fcntup == undefined ->
            % first frame after join
            case FCnt of
                N when N == 0; N == 1 ->
                    % some device start with 0, some with 1
                    {ok, uplink, Node#node{fcntup = N}};
                N when N < ?MAX_FCNT_GAP ->
                    lorawan_utils:throw_warning({node, Node#node.devaddr}, {uplinks_missed, N-1}),
                    {ok, uplink, Node#node{fcntup = N}};
                _BigN ->
                    {error, {fcnt_gap_too_large, FCnt}, Node#node.last_rx}
            end;
        (Profile#profile.fcnt_check == 2 orelse Profile#profile.fcnt_check == 3),
                FCnt < Node#node.fcntup, FCnt < MaxLost ->
            lager:debug("~s fcnt reset", [binary_to_hex(Node#node.devaddr)]),
            reset_node(Node#node.devaddr),
            % works for 16b only since we cannot distinguish between reset and 32b rollover
            {ok, uplink, Node#node{fcntup = FCnt, fcntdown=0,
                adr_use=initial_adr(Network), adr_failed=[],
                dcycle_use=Network#network.dcycle_init,
                rxwin_use=Network#network.rxwin_init, rxwin_failed=[],
                last_reset=calendar:universal_time(), devstat_fcnt=undefined, last_qs=[]}};
        Profile#profile.fcnt_check == 3, FCnt == 0 ->
            % somebody might be constantly reseting the device
            {ok, uplink, Node#node{fcntup = 0, fcntdown = 0}};
        Profile#profile.fcnt_check == 3 ->
            % checks disabled
            {ok, uplink, Node#node{fcntup = FCnt}};
        FCnt == Node#node.fcntup ->
            % retransmission
            {ok, retransmit, Node};
        Profile#profile.fcnt_check == 1 ->
            % strict 32-bit
            case fcnt32_gap(Node#node.fcntup, FCnt) of
                1 ->
                    {ok, uplink, Node#node{fcntup = fcnt32_inc(Node#node.fcntup, 1)}};
                N when N < ?MAX_FCNT_GAP ->
                    lorawan_utils:throw_warning({node, Node#node.devaddr}, {uplinks_missed, N-1}),
                    {ok, uplink, Node#node{fcntup = fcnt32_inc(Node#node.fcntup, N)}};
                _BigN ->
                    {error, {fcnt_gap_too_large, FCnt}, Node#node.last_rx}
            end;
        true ->
            % strict 16-bit (default)
            case fcnt16_gap(Node#node.fcntup, FCnt) of
                1 ->
                    {ok, uplink, Node#node{fcntup = FCnt}};
                N when N < ?MAX_FCNT_GAP ->
                    lorawan_utils:throw_warning({node, Node#node.devaddr}, {uplinks_missed, N-1}),
                    {ok, uplink, Node#node{fcntup = FCnt}};
                _BigN ->
                    {error, {fcnt_gap_too_large, FCnt}, Node#node.last_rx}
            end
    end.

fcnt16_gap(A, B) ->
    if
        A =< B -> B - A;
        A > B -> 16#FFFF - A + B
    end.

fcnt32_gap(A, B) ->
    A16 = A band 16#FFFF,
    if
        A16 > B -> 16#10000 - A16 + B;
        true  -> B - A16
    end.

fcnt32_inc(FCntUp, N) ->
    % L#node.fcntup is 32b, but the received FCnt may be 16b only
    (FCntUp + N) band 16#FFFFFFFF.

ensure_used_fields(Network, Node) ->
    ensure_adr(Network,
        ensure_rxwin(Network, Node)).

ensure_adr(#network{init_chans=InitChans, max_power=MaxPower}, Node) ->
    case Node#node.adr_use of
        {TXPower, DataRate, Chans}
                when is_integer(TXPower), is_integer(DataRate), is_list(Chans) ->
            Node;
        _Else ->
            lager:warning("~p ADR initialized", [binary_to_hex(Node#node.devaddr)]),
            Node#node{adr_use={MaxPower, 0, InitChans}}
    end.

ensure_rxwin(#network{rxwin_init=WinInit}, Node) ->
    case Node#node.rxwin_use of
        {OffSet, RX2DataRate, Frequency}
                when is_integer(OffSet), is_integer(RX2DataRate), is_number(Frequency) ->
            Node;
        _Else ->
            lager:warning("~p RXWindow initialized", [binary_to_hex(Node#node.devaddr)]),
            Node#node{rxwin_use=WinInit}
    end.


handle_accept(Gateways, {Network, Profile, Device}, DevAddr, DevNonce) ->
    AppNonce = crypto:strong_rand_bytes(3),
    {atomic, Node} = mnesia:transaction(
        fun() ->
            create_node(Gateways, {Network, Profile, Device}, AppNonce, DevAddr, DevNonce)
        end),
    reset_node(Node#node.devaddr),
    encode_accept(Network, Device, Node, AppNonce).

create_node(Gateways, {#network{netid=NetID}=Network, Profile, #device{deveui=DevEUI, appkey=AppKey}},
        AppNonce, DevAddr, DevNonce) ->
    NwkSKey = crypto:block_encrypt(aes_ecb, AppKey,
        padded(16, <<16#01, AppNonce/binary, NetID/binary, DevNonce/binary>>)),
    AppSKey = crypto:block_encrypt(aes_ecb, AppKey,
        padded(16, <<16#02, AppNonce/binary, NetID/binary, DevNonce/binary>>)),

    [Device] = mnesia:read(device, DevEUI, write),
    Device2 = append_join({calendar:universal_time(), DevNonce}, Device#device{node=DevAddr}),
    ok = mnesia:write(Device2),

    lorawan_utils:throw_info({device, DevEUI}, {join, binary_to_hex(DevAddr)}),
    Node = #node{
        devaddr=DevAddr,
        profile=Device2#device.profile, appargs=Device2#device.appargs,
        nwkskey=NwkSKey, appskey=AppSKey, desc=Device2#device.desc,
        fcntup=undefined, fcntdown=0, last_reset=calendar:universal_time(),
        gateways=Gateways, adr_flag=0, adr_set=undefined,
        adr_use=initial_adr(Network), adr_failed=[],
        dcycle_use=Network#network.dcycle_init,
        rxwin_use=accept_rxwin(Profile, Network), rxwin_failed=[],
        devstat_fcnt=undefined, last_qs=[]},
    Node2 =
        case mnesia:read(node, DevAddr, write) of
            [#node{location=Location, first_reset=First, reset_count=Cnt, last_rx=undefined, devstat=Stats}]
                    when is_integer(Cnt) ->
                lorawan_utils:throw_warning({node, DevAddr}, {repeated_reset, Cnt+1}, First),
                Node#node{location=Location, reset_count=Cnt+1, devstat=Stats};
            [#node{location=Location, devstat=Stats}] ->
                Node#node{location=Location, first_reset=calendar:universal_time(), reset_count=0, devstat=Stats};
            [] ->
                Node#node{first_reset=calendar:universal_time(), reset_count=0, devstat=[]}
        end,
    ok = lorawan_admin:write(Node2),
    Node2.

append_join(Item, #device{last_joins=undefined}=Device) ->
    Device#device{
        last_joins=[Item]};
append_join(Item, #device{last_joins=List}=Device) ->
    Device#device{
        last_joins=lists:sublist([Item | List], 5)}.

initial_adr(#network{init_chans=Chans, max_power=MaxPower}) ->
    {MaxPower, 0, Chans}.

% values that can be set directly via Join Accept's DLsettings
accept_rxwin(#profile{rxwin_set={A1,B1,_}}, #network{rxwin_init={A2,B2,C}}) ->
    {if
        is_integer(A1) -> A1;
        true -> A2
    end,
    if
        is_integer(B1) -> B1;
        true -> B2
    end,
    C};
accept_rxwin(_Else, ABC) ->
    ABC.

encode_accept(#network{netid=NetID, rx1_delay=RxDelay, cflist=CFList}, #device{appkey=AppKey},
        #node{devaddr=DevAddr, rxwin_use={RX1DROffset, RX2DataRate, _}}=Node, AppNonce) ->
    lager:debug("Join-Accept ~p, netid ~p, cflist ~p, rx1droff ~p, rx2dr ~p, appkey ~p, appnce ~p",
        [binary_to_hex(DevAddr), NetID, CFList, RX1DROffset, RX2DataRate,
        binary_to_hex(AppKey), binary_to_hex(AppNonce)]),
    MHDR = <<2#001:3, 0:3, 0:2>>,
    MACPayload = <<AppNonce/binary, NetID/binary, (reverse(DevAddr))/binary, 0:1,
        RX1DROffset:3, RX2DataRate:4, RxDelay, (encode_cflist(CFList))/binary>>,
    MIC = aes_cmac:aes_cmac(AppKey, <<MHDR/binary, MACPayload/binary>>, 4),

    % yes, decrypt; see LoRaWAN specification, Section 6.2.5
    PHYPayload = crypto:block_decrypt(aes_ecb, AppKey, padded(16, <<MACPayload/binary, MIC/binary>>)),
    {ok, Node, <<MHDR/binary, PHYPayload/binary>>}.

encode_cflist(List) when is_list(List), length(List) > 0, length(List) =< 5 ->
    FreqList =
        lists:foldr(
            fun
                ({Freq, _, _}, Acc) -> encode_cf(Freq, Acc);
                % backwards compatibility
                (Freq, Acc) -> encode_cf(Freq, Acc)
            end,
            <<>>, List),
    padded(16, FreqList);
encode_cflist(_Else) ->
    <<>>.

encode_cf(Freq, Acc) ->
    <<(trunc(Freq*10000)):24/little-unsigned-integer, Acc/binary>>.

encode_unicast({_Network, #profile{adr_mode=ADR},
        #node{devaddr=DevAddr, nwkskey=NwkSKey, appskey=AppSKey}}, ACK, FOpts, TxData) ->
    {atomic, #node{fcntdown=FCntDown}=D} = mnesia:transaction(
        fun() ->
            [D] = mnesia:read(node, DevAddr, write),
            FCnt = (D#node.fcntdown + 1) band 16#FFFFFFFF,
            NewD = D#node{fcntdown=FCnt},
            ok = lorawan_admin:write(NewD),
            NewD
        end),
    {ok, D, encode_frame(DevAddr, NwkSKey, AppSKey, FCntDown, get_adr_flag(ADR), ACK, FOpts, TxData)}.

encode_multicast(DevAddr, TxData) ->
    {atomic, #multicast_channel{fcntdown=FCntDown, nwkskey=NwkSKey, appskey=AppSKey}=G} =
        mnesia:transaction(
            fun() ->
                [D] = mnesia:read(multicast_channel, DevAddr, write),
                FCnt = (D#multicast_channel.fcntdown + 1) band 16#FFFFFFFF,
                NewD = D#multicast_channel{fcntdown=FCnt},
                ok = mnesia:write(NewD),
                NewD
            end),
    {ok, G, encode_frame(DevAddr, NwkSKey, AppSKey, FCntDown, 0, 0, <<>>, TxData)}.

get_adr_flag(ADR) when ADR == undefined; ADR == 0 -> 0;
get_adr_flag(ADR) when ADR > 0 -> 1.

encode_frame(DevAddr, NwkSKey, _AppSKey, FCnt, ADR, ACK, FOpts,
        #txdata{port=0, data=Data, confirmed=Confirmed, pending=FPending}) ->
    FHDR = <<(reverse(DevAddr)):4/binary, ADR:1, 0:1, ACK:1, (bool_to_pending(FPending)):1, 0:4,
        FCnt:16/little-unsigned-integer>>,
    FRMPayload = cipher(FOpts, NwkSKey, 1, DevAddr, FCnt),
    MACPayload = <<FHDR/binary, 0:8, (reverse(FRMPayload))/binary>>,
    if
        Data == undefined; Data == <<>> -> ok;
        true -> lager:warning("Ignored application data with Port 0")
    end,
    sign_frame(Confirmed, DevAddr, NwkSKey, FCnt, MACPayload);

encode_frame(DevAddr, NwkSKey, AppSKey, FCnt, ADR, ACK, FOpts,
        #txdata{port=Port, data=Data, confirmed=Confirmed, pending=FPending}) ->
    FHDR = <<(reverse(DevAddr)):4/binary, ADR:1, 0:1, ACK:1, (bool_to_pending(FPending)):1, (byte_size(FOpts)):4,
        FCnt:16/little-unsigned-integer, FOpts/binary>>,
    MACPayload = case Port of
        undefined when Data == undefined; Data == <<>> ->
            <<FHDR/binary>>;
        undefined ->
            lager:warning("Ignored application data without a Port number"),
            <<FHDR/binary>>;
        Num when Num > 0 ->
            FRMPayload = cipher(Data, AppSKey, 1, DevAddr, FCnt),
            <<FHDR/binary, Port:8, (reverse(FRMPayload))/binary>>
    end,
    sign_frame(Confirmed, DevAddr, NwkSKey, FCnt, MACPayload).

sign_frame(Confirmed, DevAddr, NwkSKey, FCnt, MACPayload) ->
    MType =
        case Confirmed of
            false -> 2#011;
            true -> 2#101
        end,
    Msg = <<MType:3, 0:3, 0:2, MACPayload/binary>>,
    MIC = aes_cmac:aes_cmac(NwkSKey, <<(b0(1, DevAddr, FCnt, byte_size(Msg)))/binary, Msg/binary>>, 4),
    <<Msg/binary, MIC/binary>>.

bool_to_pending(true) -> 1;
bool_to_pending(false) -> 0;
bool_to_pending(undefined) -> 0.


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

padded(Bytes, Msg) ->
    case bit_size(Msg) rem (8*Bytes) of
        0 -> Msg;
        N -> <<Msg/bitstring, 0:(8*Bytes-N)>>
    end.

% end of file
