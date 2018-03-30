%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_backend).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_uplink/4, handle_rxq/5, handle_delivery/3]).
-export([handle_downlink/2]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

init(_App) ->
    ok.

handle_join({_Network, #profile{app=AppID}, Device}, {_MAC, _RxQ}, DevAddr) ->
    case mnesia:dirty_read(handlers, AppID) of
        [Handler] ->
            send_event(joined, #{}, Handler, {Device, DevAddr});
        [] ->
            {error, {unknown_application, AppID}}
    end.

handle_uplink({Network, #profile{app=AppID}, #node{devaddr=DevAddr}=Node}, _RxQ, LastMissed, Frame) ->
    case mnesia:dirty_read(handlers, AppID) of
        [#handler{downlink_expires=Expires}=Handler] ->
            case LastMissed of
                {missed, _Receipt} ->
                    case lorawan_application:get_stored_frames(DevAddr) of
                        [] ->
                            retransmit;
                        List when length(List) > 0, Expires == <<"never">> ->
                            retransmit;
                        _Else ->
                            handle_uplink0(Handler, Network, Node, Frame)
                    end;
                undefined ->
                    handle_uplink0(Handler, Network, Node, Frame)
            end;
        [] ->
            {error, {unknown_application, AppID}}
    end.

handle_uplink0(#handler{app=AppID, uplink_fields=Fields}=Handler, Network, Node, Frame) ->
    Vars = parse_uplink(Handler, Network, Node, Frame),
    case any_is_member([<<"freq">>, <<"datr">>, <<"codr">>, <<"best_gw">>,
            <<"mac">>, <<"lsnr">>, <<"rssi">>, <<"all_gw">>], Fields) of
        true ->
            % we have to wait for the rx quality indicators
            {ok, {Handler, Vars}};
        false ->
            lorawan_backend_factory:uplink(AppID, Node, Vars),
            {ok, undefined}
    end.

handle_rxq({_Network, _Profile, #node{devaddr=DevAddr}},
        _Gateways, _WillReply, #frame{port=Port}, undefined) ->
    % we did already handle this uplink
    lorawan_application:send_stored_frames(DevAddr, Port);
handle_rxq({_Network, #profile{app=AppID}, #node{devaddr=DevAddr}=Node},
        Gateways, _WillReply, #frame{port=Port}, {#handler{uplink_fields=Fields}, Vars}) ->
    lorawan_backend_factory:uplink(AppID, Node, parse_rxq(Gateways, Fields, Vars)),
    lorawan_application:send_stored_frames(DevAddr, Port).

any_is_member(_List1, undefined) ->
    false;
any_is_member(List1, List2) ->
    lists:any(
        fun(Item1) ->
            lists:member(Item1, List2)
        end,
        List1).

parse_uplink(#handler{app=AppID, payload=Payload, parse_uplink=Parse, uplink_fields=Fields},
        #network{netid=NetID},
        #node{appargs=AppArgs, devstat=DevStat},
        #frame{devaddr=DevAddr, fcnt=FCnt, port=Port, data=Data}) ->
    Vars =
        vars_add(netid, NetID, Fields,
        vars_add(app, AppID, Fields,
        vars_add(devaddr, DevAddr, Fields,
        vars_add(deveui, get_deveui(DevAddr), Fields,
        vars_add(appargs, AppArgs, Fields,
        vars_add(battery, get_battery(DevStat), Fields,
        vars_add(fcnt, FCnt, Fields,
        vars_add(port, Port, Fields,
        vars_add(data, Data, Fields,
        vars_add(datetime, calendar:universal_time(), Fields,
        parse_payload(Payload, Data))))))))))),
    data_to_fields(AppID, Parse, Vars, Data).

parse_rxq(Gateways, Fields, Vars) ->
    {MAC1, #rxq{freq=Freq, datr=Datr, codr=Codr, rssi=RSSI1, lsnr=SNR1}} = hd(Gateways),
    RxQ =
        lists:map(
            fun({MAC, #rxq{time=Time, rssi=RSSI, lsnr=SNR}}) ->
                #{mac=>MAC, rssi=>RSSI, lsnr=>SNR, time=>Time}
            end,
            Gateways),
    vars_add(freq, Freq, Fields,
        vars_add(datr, Datr, Fields,
        vars_add(codr, Codr, Fields,
        vars_add(best_gw, hd(RxQ), Fields,
        vars_add(mac, MAC1, Fields,
        vars_add(lsnr, SNR1, Fields,
        vars_add(rssi, RSSI1, Fields,
        vars_add(all_gw, RxQ, Fields,
        Vars)))))))).

vars_add(_Field, Value, Fields, Vars)
        when Value == undefined; Fields == undefined ->
    Vars;
vars_add(Field, Value, Fields, Vars) ->
    case lists:member(atom_to_binary(Field, latin1), Fields) of
        true ->
            Vars#{Field => Value};
        false ->
            Vars
    end.

get_deveui(DevAddr) ->
    case mnesia:dirty_index_read(devices, DevAddr, #device.node) of
        [#device{deveui=DevEUI}|_] -> DevEUI;
        [] -> undefined
    end.

get_battery([{_DateTime, Battery, _Margin, _MaxSNR}|_]) ->
    Battery;
get_battery(_Else) ->
    undefined.

data_to_fields(AppId, {_, Fun}, Vars, Data) when is_function(Fun) ->
    try Fun(Vars, Data)
    catch
        Error:Term ->
            lorawan_utils:throw_error({handler, AppId}, {parse_failed, {Error, Term}}),
            Vars
    end;
data_to_fields(_AppId, _Else, Vars, _) ->
    Vars.

handle_delivery({_Network, #profile{app=AppID}, Node}, Result, Receipt) ->
    case mnesia:dirty_read(handlers, AppID) of
        [Handler] ->
            send_event(Result, #{receipt => Receipt}, Handler, Node);
        [] ->
            {error, {unknown_application, AppID}}
    end.

send_event(Event, Vars0, #handler{app=AppID, event_fields=Fields, parse_event=Parse}, DeviceOrNode) ->
    Vars =
        vars_add(app, AppID, Fields,
        vars_add(event, Event, Fields,
        vars_add(datetime, calendar:universal_time(), Fields,
        case DeviceOrNode of
            {#device{deveui=DevEUI, appargs=AppArgs}, DevAddr} ->
                vars_add(devaddr, DevAddr, Fields,
                vars_add(deveui, DevEUI, Fields,
                vars_add(appargs, AppArgs, Fields,
                Vars0)));
            #node{devaddr=DevAddr, appargs=AppArgs} ->
                vars_add(devaddr, DevAddr, Fields,
                vars_add(deveui, get_deveui(DevAddr), Fields,
                vars_add(appargs, AppArgs, Fields,
                Vars0)))
        end))),
    lorawan_backend_factory:event(AppID, DeviceOrNode,
        data_to_fields(AppID, Parse, Vars, Event)).

handle_downlink(AppId, Vars) ->
    [#handler{build=Build}=Handler] = mnesia:dirty_read(handlers, AppId),
    send_downlink(Handler,
        Vars,
        maps:get(time, Vars, undefined),
        #txdata{
            confirmed = maps:get(confirmed, Vars, false),
            port = maps:get(port, Vars, undefined),
            data = fields_to_data(AppId, Build, Vars),
            pending = maps:get(pending, Vars, undefined),
            receipt = maps:get(receipt, Vars, undefined)
        }).

fields_to_data(AppId, {_, Fun}, Vars) when is_function(Fun) ->
    try Fun(Vars)
    catch
        Error:Term ->
            lorawan_utils:throw_error({handler, AppId}, {build_failed, {Error, Term}}),
            <<>>
    end;
fields_to_data(_AppId, _Else, Vars) ->
    maps:get(data, Vars, <<>>).

send_downlink(Handler, #{deveui := DevEUI}, undefined, TxData) ->
    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            {error, {{device, DevEUI}, unknown_deveui}};
        [Device] ->
            [Node] = mnesia:dirty_read(nodes, Device#device.node),
            % standard downlink to an explicit node
            purge_frames(Handler, Node, TxData),
            lorawan_application:store_frame(Device#device.node, TxData)
    end;
send_downlink(Handler, #{deveui := DevEUI}, Time, TxData) ->
    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            {error, {{device, DevEUI}, unknown_deveui}};
        [Device] ->
            [Node] = mnesia:dirty_read(nodes, Device#device.node),
            % class C downlink to an explicit node
            purge_frames(Handler, Node, TxData),
            lorawan_handler:downlink(Node, Time, TxData)
    end;
send_downlink(Handler, #{devaddr := DevAddr}, undefined, TxData) ->
    case mnesia:dirty_read(nodes, DevAddr) of
        [] ->
            {error, {{node, DevAddr}, unknown_devaddr}};
        [Node] ->
            % standard downlink to an explicit node
            purge_frames(Handler, Node, TxData),
            lorawan_application:store_frame(DevAddr, TxData)
    end;
send_downlink(Handler, #{devaddr := DevAddr}, Time, TxData) ->
    case mnesia:dirty_read(nodes, DevAddr) of
        [] ->
            case mnesia:dirty_read(multicast_channels, DevAddr) of
                [] ->
                    {error, {{node, DevAddr}, unknown_devaddr}};
                [Group] ->
                    % scheduled multicast
                    lorawan_handler:multicast(Group, Time, TxData)
            end;
        [Node] ->
            % class C downlink to an explicit node
            purge_frames(Handler, Node, TxData),
            lorawan_handler:downlink(Node, Time, TxData)
    end;
send_downlink(Handler, #{app := AppID}, undefined, TxData) ->
    % downlink to a group
    filter_group_responses(AppID,
        lists:map(
            fun(#node{devaddr=DevAddr}=Node) ->
                purge_frames(Handler, Node, TxData),
                lorawan_application:store_frame(DevAddr, TxData)
            end,
            lorawan_backend_factory:nodes_with_backend(AppID)));
send_downlink(Handler, #{app := AppID}, Time, TxData) ->
    % class C downlink to a group of devices
    filter_group_responses(AppID,
        lists:map(
            fun(Node) ->
                purge_frames(Handler, Node, TxData),
                lorawan_handler:downlink(Node, Time, TxData)
            end,
            lorawan_backend_factory:nodes_with_backend(AppID)));
send_downlink(_Handler, Else, _Time, _TxData) ->
    lager:error("Unknown downlink target: ~p", [Else]).

purge_frames(#handler{downlink_expires = <<"superseded">>}=Handler,
        #node{devaddr=DevAddr}=Node, #txdata{port=Port}) ->
    lists:foreach(
        fun
            (#txdata{confirmed=true, receipt=Receipt}) ->
                lorawan_utils:throw_error({node, DevAddr}, downlink_lost),
                send_event(lost, #{receipt => Receipt}, Handler, Node);
            (#txdata{}) ->
                ok
        end,
        lorawan_application:take_previous_frames(DevAddr, Port));
purge_frames(_Handler, _Node, _TxData) ->
    ok.

filter_group_responses(AppID, []) ->
    lager:warning("Group ~w is empty", [AppID]);
filter_group_responses(_AppID, List) ->
    lists:foldl(
        fun (ok, Right) -> Right;
            (Left, _) -> Left
        end,
        ok, List).

parse_payload(<<"ascii">>, Data) ->
    #{text => Data};
parse_payload(<<"cayenne">>, Data) ->
    cayenne_decode(Data);
% TODO: IEEE1888 decode parse
% parse_payload(<<"ieee1888">>, Data) ->
%    ieee1888_decode(Data);
parse_payload(None, _Data) when None == <<>>; None == undefined ->
    #{};
parse_payload(Else, _Data) ->
    lager:error("Unknown payload: ~p", [Else]),
    #{}.

cayenne_decode(Bin) ->
    cayenne_decode(Bin, #{}).

% digital input
cayenne_decode(<<Ch, 0, Val, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val, Acc));
% digital output
cayenne_decode(<<Ch, 1, Val, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val, Acc));
% analog input
cayenne_decode(<<Ch, 2, Val:16/signed-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/100, Acc));
% analog output
cayenne_decode(<<Ch, 3, Val:16/signed-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/100, Acc));
% illuminance
cayenne_decode(<<Ch, 101, Val:16/unsigned-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val, Acc));
% presence
cayenne_decode(<<Ch, 102, Val, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val, Acc));
% temperature
cayenne_decode(<<Ch, 103, Val:16/signed-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/10, Acc));
% humidity
cayenne_decode(<<Ch, 104, Val, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/2, Acc));
% accelerometer
cayenne_decode(<<Ch, 113, X:16/signed-integer, Y:16/signed-integer, Z:16/signed-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, #{x => X/1000, y => Y/1000, z => Z/1000}, Acc));
% barometer
cayenne_decode(<<Ch, 115, Val:16/unsigned-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/10, Acc));
% voltage
cayenne_decode(<<Ch, 116, Val:16/unsigned-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/10, Acc));
% current
cayenne_decode(<<Ch, 117, Val:16/unsigned-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/10, Acc));
% percentage
cayenne_decode(<<Ch, 120, Val/signed-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val, Acc));
% prssure
cayenne_decode(<<Ch, 123, Val:16/unsigned-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/10, Acc));
% power
cayenne_decode(<<Ch, 128, Val:16/signed-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/10, Acc));
% energy
cayenne_decode(<<Ch, 131, Val:16/signed-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val/10, Acc));
% direction 
cayenne_decode(<<Ch, 132, Val/unsigned-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, Val, Acc));
% gyrometer
cayenne_decode(<<Ch, 134, X:16/signed-integer, Y:16/signed-integer, Z:16/signed-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, #{x => X/100, y => Y/100, z => Z/100}, Acc));
% gps
cayenne_decode(<<Ch, 136, Lat:24/signed-integer, Lon:24/signed-integer, Alt:24/signed-integer, Rest/binary>>, Acc) ->
    cayenne_decode(Rest, add_field(Ch, #{lat => Lat/10000, lon => Lon/10000, alt => Alt/100}, Acc));
cayenne_decode(<<>>, Acc) ->
    Acc.

add_field(Num, Value, Acc) ->
    maps:put(<<"field", (integer_to_binary(Num))/binary>>, Value, Acc).

-include_lib("eunit/include/eunit.hrl").

% https://github.com/myDevicesIoT/cayenne-docs/blob/master/docs/LORA.md
cayenne_test_()-> [
    ?_assertEqual(#{<<"field3">> => 27.2, <<"field5">> => 25.5},
        cayenne_decode(lorawan_utils:hex_to_binary(<<"03670110056700FF">>))),
    ?_assertEqual(#{<<"field1">> => -4.1},
        cayenne_decode(lorawan_utils:hex_to_binary(<<"0167FFD7">>))),
    ?_assertEqual(#{<<"field6">> => #{x => 1.234, y => -1.234, z => 0.0}},
        cayenne_decode(lorawan_utils:hex_to_binary(<<"067104D2FB2E0000">>))),
    ?_assertEqual(#{<<"field1">> => #{lat => 42.3519, lon => -87.9094, alt => 10.0}},
        cayenne_decode(lorawan_utils:hex_to_binary(<<"018806765ff2960a0003e8">>)))
].

% end of file
