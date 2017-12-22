%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_backend).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_uplink/4, handle_rxq/4]).
-export([parse_uplink/5, handle_downlink/3, form_encode/1]).

-include("lorawan_application.hrl").
-include("lorawan.hrl").

init(_App) ->
    ok.

handle_join({Network, Profile, Device}, {MAC, RxQ}, DevAddr) ->
    % accept any device
    ok.

handle_uplink({Network, Profile, Node}, _RxQ, {lost, State}, Frame) ->
    retransmit;
handle_uplink({Network, #profile{app=App}=Profile, Node}, _RxQ, _LastAcked, Frame) ->
    case mnesia:dirty_read(handlers, App) of
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
            {error, {unknown_appid, AppID}}
    end.

handle_rxq(_Context, _Gateways, _Frame, undefined) ->
    % we did already handle this
    ok;
handle_rxq({Network, Profile, Node}, Gateways, Frame, {#handler{fields=Fields}=Handler, Vars}) ->
    Vars2 = parse_rxq(Gateways, Fields, Vars),
    lorawan_backend_factory:uplink({Profile, Node, Handler}, Vars2),
    lorawan_application:send_stored_frames(DevAddr, Port).

any_is_member(List1, List2) ->
    lists:any(
        fun(Item1) ->
            lists:member(Item1, List2)
        end,
        List2).

parse_uplink(#handler{appid=AppID, parse=Parse, fields=Fields},
        #frame{devaddr=DevAddr, fcnt=FCnt, fport=Port, data=Data}) ->
    Vars =
        vars_add(devaddr, DevAddr, Fields,
        vars_add(deveui, get_deveui(DevAddr), Fields,
        vars_add(fcnt, FCnt, Fields,
        vars_add(port, Port, Fields,
        vars_add(data, Data, Fields,
        vars_add(datetime, calendar:universal_time(), Fields,
        #{}))))),
    data_to_fields(Parse, Vars, Data);

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
    try Fun(Port, Data)
    catch
        Error:Term ->
            lorawan_utils:throw_error({handler, AppId}, {parse_failed, {Error, Term}}),
            Vars
    end;
data_to_fields(_Else, Vars, _) ->
    Vars.


handle_downlink(Msg, Format, Vars) ->
    Handler = get_handler(get_appid(Vars), Format),
    case build_downlink(Handler, Msg) of
        {ok, Vars2, Time, TxData} ->
            send_downlink(maps:merge(Vars, Vars2), Time, TxData);
        {error, Error} ->
            {error, Error}
    end.

get_appid(#{deveui := DevEUI} = Vars) ->
    get_device_appid(DevEUI, Vars);
get_appid(#{devaddr := DevAddr} = Vars) ->
    get_link_appid(DevAddr, Vars);
get_appid(Vars) ->
    maps:get(group, Vars, undefined).

get_device_appid(DevEUI, Vars) ->
    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            case maps:get(devaddr, Vars, undefined) of
                undefined ->
                    maps:get(group, Vars, undefined);
                DevAddr ->
                    get_link_appid(DevAddr, Vars)
            end;
        [Device] ->
            Device#device.appid
    end.

get_link_appid(DevAddr, Vars) ->
    case mnesia:dirty_read(nodes, DevAddr) of
        [] ->
            case mnesia:dirty_read(multicast_groups, DevAddr) of
                [] ->
                    maps:get(group, Vars, undefined);
                [Group] ->
                    Group#multicast_channel.appid
            end;
        [Link] ->
            Link#node.appid
    end.

get_handler(AppID, Format) ->
    case mnesia:dirty_read(handlers, AppID) of
        [] ->
            #handler{format=Format};
        [Handler] when Format == undefined ->
            Handler;
        [Handler] ->
            Handler#handler{format=Format}
    end.



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
send_downlink(#{group := AppID}, undefined, TxData) ->
    % downlink to a group
    filter_group_responses(AppID,
        [lorawan_application:store_frame(DevAddr, TxData)
            || DevAddr <- mnesia:dirty_select(nodes, [{#node{devaddr='$1', appid=AppID, _='_'}, [], ['$1']}])]
    );
send_downlink(#{group := AppID}, Time, TxData) ->
    % class C downlink to a group of devices
    filter_group_responses(AppID,
        [lorawan_application:downlink(Link, Time, TxData)
            || Link <- mnesia:dirty_select(nodes, [{#node{appid=AppID, _='_'}, [], ['$_']}])]
    );
send_downlink(Else, _Time, _TxData) ->
    lager:error("Unknown downlink target: ~p", [Else]).

filter_group_responses(AppID, []) ->
    lager:warning("Group ~w is empty", [AppID]);
filter_group_responses(_AppID, List) ->
    lists:foldl(
        fun (ok, Right) -> Right;
            (Left, _) -> Left
        end,
        ok, List).

build_downlink(#handler{format = <<"json">>, build = Build}, Msg) ->
    case catch lorawan_admin:parse(jsx:decode(Msg, [return_maps, {labels, atom}])) of
        Struct when is_map(Struct) ->
            Port = maps:get(port, Struct, undefined),
            Data = maps:get(data, Struct, <<>>),
            {ok, Struct,
                maps:get(time, Struct, undefined),
                #txdata{
                    confirmed = maps:get(confirmed, Struct, false),
                    port = Port,
                    data = fields_to_data(Build, Port, maps:get(fields, Struct, #{}), Data),
                    pending = maps:get(pending, Struct, undefined)
                }};
        _Else ->
            {error, json_syntax_error}
    end;
build_downlink(#handler{build = Build}, Data) ->
    {ok, #{}, undefined,
        #txdata{data = fields_to_data(Build, undefined, #{}, Data)}}.

fields_to_data({_, Fun}, Port, Fields, Data) when is_function(Fun) ->
    try Fun(Port, Fields)
    catch
        Error:Term ->
            lager:error("Fun failed ~w:~p", [Error, Term]),
            Data
    end;
fields_to_data(_Else, _, _, Data) ->
    Data.

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
