%
% Copyright (c) 2016-2019 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_gw_lns).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

-define(MHZ, 1000000).

-record(state, {mac, area, peer, proto, user, pending}).

init(Req, []) ->
    MAC = lorawan_admin:parse_field(mac, cowboy_req:binding(mac, Req)),
    case authorize(Req) of
        {ok, User, AuthScopes} ->
            case lists:member(<<"gateway:link">>, AuthScopes) orelse lists:member(<<"unlimited">>, AuthScopes) of
                true ->
                    init_main(Req, MAC, #state{peer=cowboy_req:peer(Req), user=User});
                false ->
                    lager:error("User ~p does not have 'gateway:link' rights", [User]),
                    error_response(403, Req)
            end;
        unauthorized ->
            error_response(403, Req)
    end.

init_main(Req, undefined, State) ->
    {cowboy_websocket, Req, State};
init_main(Req, MAC, State) ->
    case mnesia:dirty_read(gateway, MAC) of
        [#gateway{area=Name}] ->
            case mnesia:dirty_read(area, Name) of
                [#area{}=Area] ->
                    {cowboy_websocket, Req, State#state{mac=MAC, area=Area, pending=maps:new()}};
                _ ->
                    lorawan_utils:throw_error({gateway, MAC}, unknown_area, aggregated),
                    error_response(500, Req)
            end;
        _Else ->
            lorawan_utils:throw_error({gateway, MAC}, unknown_mac, aggregated),
            error_response(404, Req)
    end.

authorize(Req) ->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
        {basic, User, Pass} ->
            case mnesia:dirty_read(user, User) of
                [#user{pass_ha1=HA1, scopes=AuthScopes}] ->
                    case lorawan_http_digest:ha1({User, ?REALM, Pass}) of
                        HA1 ->
                            {ok, User, AuthScopes};
                        _Else ->
                            unauthorized
                    end;
                [] ->
                    unauthorized
            end;
        _Else ->
            case mnesia:dirty_read(user, <<"anonymous">>) of
                [#user{scopes=AuthScopes}] ->
                    {ok, undefined, AuthScopes};
                [] ->
                    lager:error("Authorization header missing", []),
                    unauthorized
            end
    end.

error_response(Code, Req) ->
    Req2 = cowboy_req:reply(Code, Req),
    {ok, Req2, undefined}.

websocket_init(State) ->
    lager:debug("WebSocket connector", []),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    case catch jsx:decode(Msg, [return_maps, {labels, atom}]) of
        Msg2 when is_map(Msg2) ->
            %% lager:debug("LNS -> ~p", [Msg2]),
            case handle_message(Msg2, State) of
                ok ->
                    {ok, State};
                {ok, State2} ->
                    {ok, State2};
                stop ->
                    {stop, State};
                {reply, Reply, State2} ->
                    %% lager:debug("LNS <- ~p", [Reply]),
                    {reply, {text, jsx:encode(Reply)}, State2}
            end;
        _ ->
            lager:error("Bad JSON in LNS message"),
            {stop, State}
    end;
websocket_handle({ping, _}, State) ->
    % TODO: we should maintain keep-alive messages
    {ok, State};
websocket_handle(Data, State) ->
    lager:warning("Unknown handle ~w", [Data]),
    {ok, State}.

handle_message(#{router:=Router}, #state{mac=undefined}=State) ->
    MAC = lorawan_eid:parse(Router),
    ID6 = lorawan_eid:as_id6(MAC),
    % determine own address
    [#config{admin_url=Prefix}] = mnesia:dirty_read(config, <<"main">>),
    {ok, {Scheme, _, Host, Port, Path, _}} = http_uri:parse(Prefix),
    Scheme2 =
        case Scheme of
            http -> <<"ws">>;
            https -> <<"wss">>
        end,
    % construct the response
    Res = #{router => ID6, muxs => ID6,
        uri => list_to_binary([Scheme2, <<"://">>, Host, <<":">>, integer_to_binary(Port),
            Path, <<"router-info/">>, lorawan_utils:binary_to_hex(MAC)])},
    {reply, Res, State};
handle_message(#{router:=_Router}, #state{mac=MAC}) ->
    lager:error("Unexpected router-info from ~s", [lorawan_utils:binary_to_hex(MAC)]),
    stop;
handle_message(#{msgtype:=<<"version">>, station:=Station, firmware:=Firmware, package:=Package, model:=Model, protocol:=Proto},
        #state{mac=MAC, area=Area, peer={IP,Port}}=State) ->
    lager:debug("~s basicstation ~s ~s ~s ~s", [lorawan_utils:binary_to_hex(MAC), Station, Firmware, Package, Model]),
    lorawan_gw_router:alive(MAC, self(), {IP, Port, Proto}),
    {ok, _} = timer:send_interval(15000, ping),
    {reply, router_config(Area, MAC), State#state{proto=Proto}};
handle_message(#{msgtype:=<<"jreq">>, 'MHdr':=MHdr, 'JoinEui':=JoinEUI0, 'DevEui':=DevEUI0, 'DevNonce':=DevNonce, 'MIC':=MIC}=Msg,
        #state{mac=MAC, area=#area{region=Region}}) ->
    {RxQ, GWState} = parse_rxq(Region, Msg),
    % the basicstation was so kind to parse the payload for us
    % sadly, we have to reconstruct the original frame for the MIC check
    AppEUI = lorawan_utils:reverse(lorawan_eid:parse(JoinEUI0)),
    DevEUI = lorawan_utils:reverse(lorawan_eid:parse(DevEUI0)),
    PHYPayload = <<MHdr, AppEUI:8/binary, DevEUI:8/binary,
        DevNonce:16/little-unsigned-integer, MIC:32/little-signed-integer>>,
    lorawan_gw_router:uplinks([{{MAC, RxQ, GWState}, PHYPayload}]);
handle_message(#{msgtype:=<<"updf">>, 'MHdr':=MHdr, 'DevAddr':=DevAddr, 'FCtrl':=FCtrl,
            'FCnt':=FCnt, 'FOpts':=FOpts, 'FPort':=FPort0, 'FRMPayload':=FRMPayload, 'MIC':=MIC}=Msg,
        #state{mac=MAC, area=#area{region=Region}}) ->
    handle_reftime(MAC, Msg),
    {RxQ, GWState} = parse_rxq(Region, Msg),
    FPort =
        case FPort0 of
            -1 -> <<>>;
            _ -> <<FPort0>>
        end,
    PHYPayload = <<MHdr, DevAddr:32/little-signed-integer, FCtrl,
        FCnt:16/little-unsigned-integer, (lorawan_utils:hex_to_binary(FOpts))/binary,
        FPort/binary, (lorawan_utils:hex_to_binary(FRMPayload))/binary, MIC:32/little-signed-integer>>,
    lorawan_gw_router:uplinks([{{MAC, RxQ, GWState}, PHYPayload}]);
handle_message(#{msgtype:=<<"propdf">>, 'FRMPayload':=FRMPayload}, #state{mac=MAC}) ->
    lager:debug("~s got proprietary frame ~s", [lorawan_utils:binary_to_hex(MAC), lorawan_utils:binary_to_hex(FRMPayload)]);
handle_message(#{msgtype:=<<"dntxed">>, diid:=DIID}, #state{pending=Pending}=State) ->
    Pending2 =
        case maps:take(DIID, Pending) of
            {Timer, Pndn} ->
                {ok, cancel} = timer:cancel(Timer),
                Pndn;
            error ->
                Pending
        end,
    {ok, State#state{pending=Pending2}};
handle_message(#{msgtype:=<<"timesync">>}, _State) ->
    ok; % silently ignored
handle_message(Msg, _State) ->
    lager:warning("Unexpected LNS message: ~p", [Msg]),
    stop.

websocket_info({send, _, #{rctx:=RxTx, xtime:=XTime}, DevAddr,
        #txq{freq=Freq, datr=DatR, time=Time}, _, PHYPayload},
        #state{area=#area{region=Region}, pending=Pending}=State) ->
    <<DIID:64/little-signed-integer>> = crypto:strong_rand_bytes(8),
    Reply0 = #{msgtype => <<"dnmsg">>,
        % we use DevAddr insead as ABP devices might not have a DevEUI assigned
        'DevEui' => lorawan_utils:binary_to_hex(<<0:32,DevAddr/binary>>),
        diid => DIID,
        pdu => lorawan_utils:binary_to_hex(PHYPayload),
        'DR'=>lorawan_mac_region:datar_to_dr(Region, DatR),
        'Freq'=>round(Freq*?MHZ),
        priority=>0,
        rctx=>RxTx,
        'MuxTime'=>lorawan_utils:time_to_unix()/1000
    },
    Reply =
        case Time of
            Num when is_number(Num) -> Reply0#{dC=>0, xtime=>XTime, 'RxDelay'=>Num};
            immediately -> Reply0#{dC=>2}
        end,
    {ok, Timer} = timer:send_after(30000, {no_ack, DIID, DevAddr}),
    %% lager:debug("LNS <- ~p", [Reply]),
    {reply, {text, jsx:encode(Reply)},
        State#state{pending=maps:put(DIID, Timer, Pending)}};
websocket_info({no_ack, DIID, DevAddr}, #state{mac=MAC, pending=Pending}=State) ->
    case maps:take(DIID, Pending) of
        {_, Pending2} ->
            lorawan_gw_router:downlink_error(MAC, DevAddr, {not_transmitted, DIID}),
            {ok, State#state{pending=Pending2}};
        error ->
            ok
    end;
websocket_info(ping, #state{mac=MAC, peer={IP,Port}, proto=Proto}=State) ->
    % TODO: this should be done in response to pong, but the v2 does not repond to pings
    lorawan_gw_router:alive(MAC, self(), {IP, Port, Proto}),
    {reply, ping, State};
websocket_info(Info, State) ->
    lager:warning("Unknown info ~p", [Info]),
    {ok, State}.

terminate(Reason, _Req, _State) ->
    lager:debug("WebSocket terminated: ~p", [Reason]),
    ok.


router_config(#area{region=Region}, MAC) ->
    #{min:=MinFreq, max:=MaxFreq} = lorawan_mac_region:freq(Region),
    {atomic, {NetIDs, Freq, MaxDR}} =
        mnesia:transaction(fun() ->
            mnesia:foldl(
                fun (#network{netid= <<NetID:24>>, region=Reg, max_datr=Max2}=Net,
                        {IDs, Freq1, Max1})
                    when Reg==Region ->
                        Freq2 = lorawan_mac_region:net_freqs(Net),
                        {[NetID | IDs], Freq1++Freq2, max(Max1,Max2)};
                    (#network{}, Acc) ->
                        Acc
                end,
                {[], [], 0}, network)
            end),
    % the sx1301 supports 4+4(+2 special) channels
    {Ra1Fr, Ra2Fr} =
        case lists:sort([round(F*?MHZ) || F <- Freq]) of
            List when length(List) < 5 ->
                {List, []};
            List ->
                if
                    length(List) > 8 ->
                        lager:warning("Too many frequencies: ignoring ~p", [lists:nthtail(8, List)]);
                    true ->
                        ok
                end,
                {lists:sublist(List, 1, 4), lists:sublist(List, 5, 4)}
        end,
    lager:debug("gateway ~s init radio_0: ~p, radio_1: ~p", [lorawan_utils:binary_to_hex(MAC), Ra1Fr, Ra2Fr]),
    #{msgtype => <<"router_config">>,
        'NetID' => NetIDs,
        region => Region,
        hwspec => <<"sx1301/1">>,
        freq_range => [round(MinFreq*?MHZ), round(MaxFreq*?MHZ)],
        'DRs' => lists:filtermap(
            fun ({Num, {SF,BW}, Dir}) when Num =< MaxDR ->
                    {true, [SF, BW, dir_to_dnonly(Dir)]};
                ({Num, BW, Dir}) when Num =< MaxDR ->
                    {true, [0, BW, dir_to_dnonly(Dir)]}; % FSK
                (_) ->
                    false
            end,
            lorawan_mac_region:datars(Region)),
        sx1301_conf => [maps:from_list(
                radio_setup(0, 0, Ra1Fr) ++ radio_setup(1, length(Ra1Fr), Ra2Fr)
            )]
    }.

dir_to_dnonly(up) -> 0;
dir_to_dnonly(down) -> 1;
dir_to_dnonly(updown) -> 0.

radio_setup(R, _C, []) ->
    [{id(<<"radio_">>, R), #{enable => false}}];
radio_setup(R, C, List) ->
    Frq = lists:sum(List) div length(List),
    [{id(<<"radio_">>, R), #{enable => true, freq => Frq}}
        | channel_setup(R, C, Frq, List)].

channel_setup(R, C, Frq, [First|List]) ->
    [{id(<<"chan_multiSF_">>, C), #{enable => true, 'if' => First-Frq, radio => R}}
        | channel_setup(R, C+1, Frq, List)];
channel_setup(_R, _C, _Frq, []) ->
    [].

id(Prefix, N) ->
    <<Prefix/binary, (integer_to_binary(N))/binary>>.


handle_reftime(MAC, #{'RefTime':=RefTime}) when RefTime > 1000000 ->
    lorawan_gw_router:network_delay(MAC, lorawan_utils:time_to_unix() - RefTime*1000);
handle_reftime(_, _) ->
    ok.

parse_rxq(Region, #{'DR':=DR, 'Freq':=Freq,
        upinfo:=#{rctx:=RxTx, xtime:=XTime, gpstime:=Time, rssi:=RSSI, snr:=SNR}}) ->
    {#rxq{
        freq=Freq/?MHZ,
        datr=lorawan_mac_region:dr_to_datar(Region, DR),
        codr= <<"4/5">>,
        tmms=
            if
                Time > 0 -> Time/1000;
                true -> undefined
            end,
        rssi=RSSI,
        lsnr=SNR
    },
    #{rctx=>RxTx, xtime=>XTime}}.

% end of file
