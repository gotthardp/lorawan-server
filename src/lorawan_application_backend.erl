%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_backend).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/5]).
-export([parse_uplink/5, handle_downlink/4]).

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
            {_, Data} = parse_uplink(Handler, DevAddr, AppArgs, RxData, RxQ),
            lorawan_connector_factory:publish(Handler#handler.connid, Data,
                [{devaddr, lorawan_mac:binary_to_hex(DevAddr)}, {group, AppID}]);
        [] ->
            {error, {unknown_appid, AppID}}
    end.

handle_downlink(Msg, Format, AppID, DevAddr) ->
    Handler = get_handler(get_appid(DevAddr, AppID), Format),
    case build_downlink(Handler, Msg) of
        {ok, undefined, Time, TxData} ->
            send_downlink(AppID, DevAddr, Time, TxData),
            ok;
        {ok, DevAddr2, Time, TxData} ->
            send_downlink(AppID, DevAddr2, Time, TxData),
            ok;
        {error, Error} ->
            {error, Error}
    end.

get_appid(DevAddr, Default) ->
    case mnesia:dirty_read(links, DevAddr) of
        [] ->
            case mnesia:dirty_read(multicast_groups, DevAddr) of
                [] ->
                    Default;
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

send_downlink(AppID, undefined, undefined, TxData) ->
    % downlink to a group
    [lorawan_handler:store_frame(DevAddr, TxData)
        || DevAddr <- mnesia:dirty_select(links, [{#link{devaddr='$1', appid=AppID, _='_'}, [], ['$1']}])];
send_downlink(_AppID, DevAddr, undefined, TxData) ->
    case mnesia:dirty_read(links, DevAddr) of
        [] ->
            {error, {unknown_devaddr, DevAddr}};
        [_Link] ->
            % standard downlink to an explicit node
            lorawan_handler:store_frame(DevAddr, TxData)
    end;
send_downlink(AppID, DevAddr, Time, TxData) ->
    case mnesia:dirty_read(links, DevAddr) of
        [] ->
            case mnesia:dirty_read(multicast_groups, DevAddr) of
                [] ->
                    % class C downlink to a group of devices
                    [lorawan_handler:downlink(Link, Time, TxData)
                        || Link <- mnesia:dirty_select(links, [{#link{appid=AppID, _='_'}, [], ['$_']}])];
                [Group] ->
                    % scheduled multicast
                    lorawan_handler:multicast(Group, Time, TxData)
            end;
        [Link] ->
            % class C downlink to an explicit node
            lorawan_handler:downlink(Link, Time, TxData)
    end.


parse_uplink(#handler{format = <<"raw">>}, _DevAddr, _AppArgs, #rxdata{data=Data}, _RxQ) ->
    {binary, Data};
parse_uplink(#handler{format = <<"json">>, parse = Parse}, DevAddr, AppArgs, RxData=#rxdata{port=Port, data=Data}, RxQ) ->
    Msg = lorawan_admin:build([{devaddr, DevAddr}, {appargs, AppArgs}, {rxq, RxQ},
        {fields, data_to_fields(Parse, Port, Data)} | ?to_proplist(rxdata, RxData)]),
    {text, jsx:encode(Msg)}.

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
    case jsx:is_json(Msg) of
        true ->
            Struct = lorawan_admin:parse(jsx:decode(Msg, [{labels, atom}])),
            Port = proplists:get_value(port, Struct),
            Data = proplists:get_value(data, Struct, <<>>),
            {ok, proplists:get_value(devaddr, Struct),
                proplists:get_value(time, Struct),
                #txdata{
                    confirmed = proplists:get_value(confirmed, Struct, false),
                    port = Port,
                    data = fields_to_data(Build, Port, proplists:get_value(fields, Struct), Data),
                    pending = proplists:get_value(pending, Struct, false)
                }};
        false ->
            {error, json_syntax_error}
    end;
build_downlink(_Else, Msg) ->
    {ok, undefined, undefined, #txdata{data=Msg}}.

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
