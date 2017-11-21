%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_application_backend).
-behaviour(lorawan_application).

-export([init/1, handle_join/3, handle_rx/4]).
-export([parse_uplink/5, handle_downlink/3]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

init(_App) ->
    ok.

handle_join(_Gateway, _Device, _Link) ->
    % accept any device
    ok.

handle_rx(_Gateway, _Link, #rxdata{last_lost=true}, _RxQ) ->
    retransmit;
handle_rx(Gateway, #link{devaddr=DevAddr}=Link, #rxdata{port=Port} = RxData, RxQ) ->
    case send_to_backend(Gateway, Link, RxData, RxQ) of
        ok ->
            lorawan_handler:send_stored_frames(DevAddr, Port);
        {error, Error} ->
            {error, Error}
    end.

send_to_backend(Gateway, #link{appid=AppID}=Link, RxData, RxQ) ->
    case mnesia:dirty_read(handlers, AppID) of
        [Handler] ->
            lorawan_connector_factory:publish(Handler#handler.connid,
                parse_uplink(Handler, Gateway, Link, RxData, RxQ));
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
    lager:error("Unknown downlink target: ~p", [Else]).

filter_group_responses(AppID, []) ->
    lager:warning("Group ~w is empty", [AppID]);
filter_group_responses(_AppID, List) ->
    lists:foldl(
        fun (ok, Right) -> Right;
            (Left, _) -> Left
        end,
        ok, List).

parse_uplink(#handler{appid = AppID, format = <<"raw">>, parse = Parse},
        _Gateway, #link{devaddr=DevAddr}, #rxdata{port=Port, data=Data}, _RxQ) ->
    Data2 =
        case data_to_fields(Parse, Port, Data) of
            undefined -> Data;
            B when is_binary(B) -> B
        end,
    {<<"application/octet-stream">>, Data2,
        #{group => AppID, deveui => get_deveui(DevAddr), devaddr => DevAddr}};
parse_uplink(#handler{appid = AppID, format = <<"json">>, fields = Fields, parse = Parse},
        #gateway{mac=MAC}, #link{devaddr=DevAddr, appargs=AppArgs},
        RxData=#rxdata{port=Port, data=Data}, RxQ) ->
    Msg = lorawan_admin:build(
        maps:merge(?to_map(rxdata, RxData),
            vars_add(appargs, AppArgs,
            vars_add_opt(gateway, #{mac => MAC}, Fields,
            vars_add_opt(deveui, get_deveui(DevAddr), Fields,
            vars_add_opt(datetime, calendar:universal_time(), Fields,
            vars_add_opt(rxq, RxQ, Fields,
            vars_add(fields, data_to_fields(Parse, Port, Data),
            #{group => AppID, devaddr => DevAddr}))))))
        )),
    {<<"application/json">>, jsx:encode(Msg),
        #{group => AppID, deveui => get_deveui(DevAddr), devaddr => DevAddr}};
parse_uplink(#handler{appid = AppID, format = <<"www-form">>, parse = Parse},
        #gateway{}, #link{devaddr=DevAddr, appargs=AppArgs},
        #rxdata{port=Port, data=Data}, _RxQ) ->
    Msg = vars_add(appargs, AppArgs,
            vars_from_map(data_to_fields(Parse, Port, Data))),
    {<<"application/x-www-form-urlencoded">>, form_encode(Msg),
        #{group => AppID, deveui => get_deveui(DevAddr), devaddr => DevAddr}}.

vars_add(_Field, undefined, Vars) ->
    Vars;
vars_add(Field, Value, Vars) ->
    Vars#{Field => Value}.

vars_add_opt(_Field, undefined, _Fields, Vars) ->
    Vars;
vars_add_opt(Field, Value, undefined, Vars) ->
    Vars#{Field => Value};
vars_add_opt(Field, Value, Fields, Vars) ->
    case lists:member(atom_to_binary(Field, latin1), Fields) of
        true ->
            Vars#{Field => Value};
        false ->
            Vars
    end.

vars_from_map(undefined) ->
    #{};
vars_from_map(Map) when is_map(Map) ->
    Map.

get_deveui(DevAddr) ->
    case mnesia:dirty_index_read(devices, DevAddr, #device.link) of
        [#device{deveui=DevEUI}|_] -> DevEUI;
        [] -> undefined
    end.

data_to_fields({_, Fun}, Port, Data) when is_function(Fun) ->
    try Fun(Port, Data)
    catch
        Error:Term ->
            lager:error("Fun failed ~w:~p", [Error, Term]),
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
