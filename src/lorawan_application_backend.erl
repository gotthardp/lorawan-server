%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_backend).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/5]).
-export([parse_uplink/5, handle_downlink/3]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

init(_App) ->
    ok.

handle_join(_DevAddr, _AppID, _AppArgs) ->
    % accept any device
    ok.

handle_rx(_DevAddr, _AppID, _AppArgs, #rxdata{last_lost=true}, _RxQ) ->
    retransmit;
handle_rx(DevAddr, AppID, AppArgs, #rxdata{port=Port} = RxData, RxQ) ->
    send_to_backend(DevAddr, AppID, AppArgs, RxData, RxQ),
    lorawan_handler:send_stored_frames(DevAddr, Port).

send_to_backend(DevAddr, AppID, AppArgs, RxData, RxQ) ->
    case mnesia:dirty_read(handlers, AppID) of
        [Handler] ->
            {_, Data, Vars} = parse_uplink(Handler, DevAddr, AppArgs, RxData, RxQ),
            lorawan_connector_factory:publish(Handler#handler.connid, Data, Vars);
        [] ->
            {error, {unknown_appid, AppID}}
    end.

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
    case mnesia:dirty_read(links, DevAddr) of
        [] ->
            case mnesia:dirty_read(multicast_groups, DevAddr) of
                [] ->
                    maps:get(group, Vars, undefined);
                [Group] ->
                    Group#multicast_group.appid
            end;
        [Link] ->
            Link#link.appid
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
            lorawan_handler:store_frame(Device#device.link, TxData)
    end;
send_downlink(#{deveui := DevEUI}, Time, TxData) ->
    case mnesia:dirty_read(devices, DevEUI) of
        [] ->
            {error, {{device, DevEUI}, unknown_deveui}};
        [Device] ->
            [Link] = mnesia:dirty_read(links, Device#device.link),
            % class C downlink to an explicit node
            lorawan_handler:downlink(Link, Time, TxData)
    end;
send_downlink(#{devaddr := DevAddr}, undefined, TxData) ->
    case mnesia:dirty_read(links, DevAddr) of
        [] ->
            {error, {{node, DevAddr}, unknown_devaddr}};
        [_Link] ->
            % standard downlink to an explicit node
            lorawan_handler:store_frame(DevAddr, TxData)
    end;
send_downlink(#{devaddr := DevAddr}, Time, TxData) ->
    case mnesia:dirty_read(links, DevAddr) of
        [] ->
            case mnesia:dirty_read(multicast_groups, DevAddr) of
                [] ->
                    {error, {{node, DevAddr}, unknown_devaddr}};
                [Group] ->
                    % scheduled multicast
                    lorawan_handler:multicast(Group, Time, TxData)
            end;
        [Link] ->
            % class C downlink to an explicit node
            lorawan_handler:downlink(Link, Time, TxData)
    end;
send_downlink(#{group := AppID}, undefined, TxData) ->
    % downlink to a group
    filter_group_responses(AppID,
        [lorawan_handler:store_frame(DevAddr, TxData)
            || DevAddr <- mnesia:dirty_select(links, [{#link{devaddr='$1', appid=AppID, _='_'}, [], ['$1']}])]
    );
send_downlink(#{group := AppID}, Time, TxData) ->
    % class C downlink to a group of devices
    filter_group_responses(AppID,
        [lorawan_handler:downlink(Link, Time, TxData)
            || Link <- mnesia:dirty_select(links, [{#link{appid=AppID, _='_'}, [], ['$_']}])]
    );
send_downlink(Else, _Time, _TxData) ->
    lager:error("Unknown downlink target: ~w", [Else]).

filter_group_responses(AppID, []) ->
    lager:warning("Group ~w is empty", [AppID]);
filter_group_responses(_AppID, List) ->
    lists:foldl(
        fun (ok, Right) -> Right;
            (Left, _) -> Left
        end,
        ok, List).

parse_uplink(#handler{appid = AppID, format = <<"raw">>},
        DevAddr, _AppArgs, #rxdata{data=Data}, _RxQ) ->
    {binary, Data, construct_vars(AppID, DevAddr)};
parse_uplink(#handler{appid = AppID, format = <<"json">>, parse = Parse},
        DevAddr, AppArgs, RxData=#rxdata{port=Port, data=Data}, RxQ) ->
    Vars = construct_vars(AppID, DevAddr),
    Msg = lorawan_admin:build(
        maps:merge(
            ?to_map(rxdata, RxData),
            Vars#{appargs => AppArgs, datetime => calendar:universal_time(),
                rxq => RxQ, fields => data_to_fields(Parse, Port, Data)}
        )),
    {text, jsx:encode(Msg), Vars}.

construct_vars(AppID, DevAddr) ->
    Vars = #{group => AppID, devaddr => DevAddr},
    case mnesia:dirty_index_read(devices, DevAddr, #device.link) of
        [#device{deveui=DevEUI}] -> Vars#{deveui => DevEUI};
        [] -> Vars
    end.

data_to_fields({_, Fun}, Port, Data) when is_function(Fun) ->
    try Fun(Port, Data)
    catch
        Error:Term ->
            lager:error("Fun failed ~w:~w", [Error, Term]),
            undefined
    end;
data_to_fields(_Else, _, _) ->
    undefined.

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
                    data = fields_to_data(Build, Port, maps:get(fields, Struct, undefined), Data),
                    pending = maps:get(pending, Struct, false)
                }};
        _Else ->
            {error, json_syntax_error}
    end;
build_downlink(_Else, Msg) ->
    {ok, #{}, undefined, #txdata{data=Msg}}.

fields_to_data(_, _, undefined, Data) ->
    Data;
fields_to_data({_, Fun}, Port, Fields, Data) when is_function(Fun) ->
    try Fun(Port, Fields)
    catch
        Error:Term ->
            lager:error("Fun failed ~w:~w", [Error, Term]),
            Data
    end;
fields_to_data(_Else, _, _, Data) ->
    Data.

% end of file
