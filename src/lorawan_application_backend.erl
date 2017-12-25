%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_backend).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_uplink/4, handle_rxq/4]).
-export([handle_downlink/3, form_encode/1]).

-include("lorawan.hrl").
-include("lorawan_db.hrl").

init(_App) ->
    ok.

handle_join({_Network, _Profile, _Device}, {_MAC, _RxQ}, _DevAddr) ->
    % accept any device
    ok.

handle_uplink({_Network, _Profile, _Node}, _RxQ, {lost, State}, _Frame) ->
    retransmit;
handle_uplink({_Network, #profile{app=AppID}=Profile, Node}, _RxQ, _LastAcked, Frame) ->
    case mnesia:dirty_read(handlers, AppID) of
        [#handler{fields=Fields}=Handler] ->
            Vars = parse_uplink(Handler, Frame),
            case any_is_member([<<"freq">>, <<"datr">>, <<"codr">>, <<"best_gw">>, <<"all_gw">>], Fields) of
                true ->
                    % we have to wait for the rx quality indicators
                    {ok, {Handler, Vars}};
                false ->
                    lorawan_backend_factory:uplink({Profile, Node, Handler}, Vars),
                    {ok, undefined}
            end;
        [] ->
            {error, {unknown_application, AppID}}
    end.

handle_rxq(_Context, _Gateways, _Frame, undefined) ->
    % we did already handle this
    ok;
handle_rxq({_Network, Profile, #node{devaddr=DevAddr}=Node}, Gateways, #frame{fport=Port}, {#handler{fields=Fields}=Handler, Vars}) ->
    Vars2 = parse_rxq(Gateways, Fields, Vars),
    lorawan_backend_factory:uplink({Profile, Node, Handler}, Vars2),
    lorawan_application:send_stored_frames(DevAddr, Port).

any_is_member(List1, List2) ->
    lists:any(
        fun(Item1) ->
            lists:member(Item1, List2)
        end,
        List1).

parse_uplink(#handler{app=AppID, parse=Parse, fields=Fields},
        #frame{devaddr=DevAddr, fcnt=FCnt, fport=Port, data=Data}) ->
    Vars =
        vars_add(devaddr, DevAddr, Fields,
        vars_add(deveui, get_deveui(DevAddr), Fields,
        vars_add(fcnt, FCnt, Fields,
        vars_add(port, Port, Fields,
        vars_add(data, Data, Fields,
        vars_add(datetime, calendar:universal_time(), Fields,
        #{})))))),
    data_to_fields(AppID, Parse, Vars, Data).

parse_rxq(Gateways, Fields, Vars) ->
    {_MAC, #rxq{freq=Freq, datr=Datr, codr=Codr}} = hd(Gateways),
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
        vars_add(all_gw, RxQ, Fields,
        Vars))))).

vars_add(_Field, undefined, _Fields, Vars) ->
    Vars;
vars_add(Field, Value, undefined, Vars) ->
    Vars#{Field => Value};
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

data_to_fields(AppId, {_, Fun}, Vars, Data) when is_function(Fun) ->
    try Fun(Vars, Data)
    catch
        Error:Term ->
            lorawan_utils:throw_error({handler, AppId}, {parse_failed, {Error, Term}}),
            Vars
    end;
data_to_fields(_AppId, _Else, Vars, _) ->
    Vars.


handle_downlink(AppID, Vars, Msg) ->
    [Handler] = mnesia:dirty_read(handlers, AppID),
    case build_downlink(Handler, Msg, Vars) of
        {ok, Vars2, Time, TxData} ->
            send_downlink(Vars2, Time, TxData);
        {error, Error} ->
            {error, Error}
    end.

build_downlink(#handler{app=AppID, build = Build}, Msg, Vars) ->
    case catch lorawan_admin:parse(jsx:decode(Msg, [return_maps, {labels, atom}])) of
        Struct when is_map(Struct) ->
            Port = maps:get(port, Struct, undefined),
            Data = maps:get(data, Struct, <<>>),
            {ok, maps:merge(Vars, Struct#{app=>AppID}),
                maps:get(time, Struct, undefined),
                #txdata{
                    confirmed = maps:get(confirmed, Struct, false),
                    port = Port,
                    data = fields_to_data(Build, Port, maps:get(fields, Struct, #{}), Data),
                    pending = maps:get(pending, Struct, undefined)
                }};
        _Else ->
            {error, json_syntax_error}
    end.

fields_to_data({_, Fun}, Port, Fields, Data) when is_function(Fun) ->
    try Fun(Port, Fields)
    catch
        Error:Term ->
            lager:error("Fun failed ~w:~p", [Error, Term]),
            Data
    end;
fields_to_data(_Else, _, _, Data) ->
    Data.

send_downlink(#{deveui := DevEUI}, undefined, TxData) ->
    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            {error, {{device, DevEUI}, unknown_deveui}};
        [Device] ->
            % standard downlink to an explicit node
            lorawan_application:store_frame(Device#device.node, TxData)
    end;
send_downlink(#{deveui := DevEUI}, Time, TxData) ->
    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            {error, {{device, DevEUI}, unknown_deveui}};
        [Device] ->
            [Link] = mnesia:dirty_read(nodes, Device#device.node),
            % class C downlink to an explicit node
            lorawan_application:downlink(Link, Time, TxData)
    end;
send_downlink(#{devaddr := DevAddr}, undefined, TxData) ->
    case mnesia:dirty_read(nodes, DevAddr) of
        [] ->
            {error, {{node, DevAddr}, unknown_devaddr}};
        [_Link] ->
            % standard downlink to an explicit node
            lorawan_application:store_frame(DevAddr, TxData)
    end;
send_downlink(#{devaddr := DevAddr}, Time, TxData) ->
    case mnesia:dirty_read(nodes, DevAddr) of
        [] ->
            case mnesia:dirty_read(multicast_channels, DevAddr) of
                [] ->
                    {error, {{node, DevAddr}, unknown_devaddr}};
                [Group] ->
                    % scheduled multicast
                    lorawan_application:multicast(Group, Time, TxData)
            end;
        [Link] ->
            % class C downlink to an explicit node
            lorawan_application:downlink(Link, Time, TxData)
    end;
send_downlink(#{app := AppID}, undefined, TxData) ->
    % downlink to a group
    filter_group_responses(AppID,
        [lorawan_application:store_frame(DevAddr, TxData)
            || #node{devaddr=DevAddr} <- lorawan_backend_factory:nodes_with_backend(AppID)]
    );
send_downlink(#{app := AppID}, Time, TxData) ->
    % class C downlink to a group of devices
    filter_group_responses(AppID,
        [lorawan_application:downlink(Node, Time, TxData)
            || Node <- lorawan_backend_factory:nodes_with_backend(AppID)]
    ).

filter_group_responses(AppID, []) ->
    lager:warning("Group ~w is empty", [AppID]);
filter_group_responses(_AppID, List) ->
    lists:foldl(
        fun (ok, Right) -> Right;
            (Left, _) -> Left
        end,
        ok, List).

form_encode(Values) ->
    cow_qs:qs(
        lists:map(
            fun({Name, Value}) ->
                {atom_to_binary(Name, latin1), value_to_binary(Value)}
            end,
            maps:to_list(Values))).

value_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, latin1);
value_to_binary(Num) when is_integer(Num) -> integer_to_binary(Num);
value_to_binary(Num) when is_float(Num) -> float_to_binary(Num);
value_to_binary(List) when is_list(List) -> list_to_binary(List);
value_to_binary(Bin) when is_binary(Bin) -> Bin.

-include_lib("eunit/include/eunit.hrl").

www_form_test_()-> [
    ?_assertEqual(<<>>, form_encode(#{})),
    ?_assertEqual(<<"one=1">>, form_encode(#{one=>1})),
    ?_assertEqual(<<"one=1&two=val">>, form_encode(#{one=>1,two=>"val"})),
    ?_assertEqual(<<"one=1&three=%26&two=val">>, form_encode(#{one=>1,two=>"val",three=><<"&">>}))
].

% end of file
