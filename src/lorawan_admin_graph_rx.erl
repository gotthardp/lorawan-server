%
% Copyright (c) 2016-2018 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_graph_rx).

-export([init/2]).
-export([allowed_methods/2]).
-export([is_authorized/2]).
-export([forbidden/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_rxframe/2]).

-include("lorawan_db.hrl").
-record(state, {format, scopes, auth_fields}).

% range encompassing all possible LoRaWAN bands
-define(DEFAULT_RANGE, {433, 928}).

init(Req, {Format, Scopes}) ->
    {cowboy_rest, Req, #state{format=Format, scopes=Scopes}}.

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

is_authorized(Req, #state{scopes=Scopes}=State) ->
    case lorawan_admin:handle_authorization(Req, Scopes) of
        {true, AuthFields} ->
            {true, Req, State#state{auth_fields=AuthFields}};
        Else ->
            {Else, Req, State}
    end.

forbidden(Req, #state{auth_fields=AuthFields}=State) ->
    {lorawan_admin:fields_empty(AuthFields), Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_rxframe}
    ], Req, State}.

get_rxframe(Req, State) ->
    DevAddr = cowboy_req:binding(devaddr, Req),
    ActRec = get_last_rxframes(lorawan_utils:hex_to_binary(DevAddr)),
    case ActRec of
        [#rxframe{network=Name} | _] ->
            case mnesia:dirty_read(network, Name) of
                [N] ->
                    send_array(Req, N, DevAddr, ActRec, State);
                [] ->
                    send_empty(Req, DevAddr, State)
            end;
        [] ->
            send_empty(Req, DevAddr, State)
    end.

get_last_rxframes(DevAddr) ->
    {Uplinks0, _} = lorawan_db:get_rxframes(DevAddr),
    Uplinks =
        if
            length(Uplinks0) =< 50 ->
                Uplinks0;
            true ->
                lists:sublist(Uplinks0, length(Uplinks0)-50+1, 50)
        end,
    % return frames received since the last device restart
    case mnesia:dirty_read(node, DevAddr) of
        [#node{last_reset=Reset}] when is_tuple(Reset) ->
            lists:filter(
                fun(#rxframe{datetime=FrameDate}) -> FrameDate >= Reset end,
                Uplinks);
        _Else ->
            Uplinks
    end.

send_empty(Req, DevAddr, State) ->
    {jsx:encode([{devaddr, DevAddr}, {array, []}]), Req, State}.

send_array(Req, #network{region=Region, max_eirp=MaxEIRP}, DevAddr, ActRec, #state{format=rgraph}=State) ->
    {Min, Max} = lorawan_mac_region:freq_range(Region),
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    Array = [{cols, [
                [{id, <<"fcnt">>}, {label, <<"FCnt">>}, {type, <<"number">>}],
                [{id, <<"datr">>}, {label, <<"Data Rate">>}, {type, <<"number">>}],
                [{id, <<"powe">>}, {label, <<"Power (dBm)">>}, {type, <<"number">>}],
                [{id, <<"freq">>}, {label, <<"Frequency (MHz)">>}, {type, <<"number">>}]
                ]},
            {rows, lists:map(
                fun(#rxframe{fcnt=FCnt, powe=TXPower, gateways=Gateways})
                        when is_number(TXPower), is_binary(Region) ->
                    {_MAC, #rxq{datr=DatR, freq=Freq}} = hd(Gateways),
                    [{c, [
                        [{v, FCnt}],
                        [{v, lorawan_mac_region:datar_to_dr(Region, DatR)}, {f, DatR}],
                        [{v, MaxEIRP/2-TXPower}, {f, integer_to_binary(MaxEIRP - 2*TXPower)}],
                        [{v, Freq}]
                    ]}];
                % if there are some unexpected data, show nothing
                (_Else) ->
                    [{c, []}]
                end, ActRec)
            }
        ],
    {jsx:encode([{devaddr, DevAddr}, {array, Array}, {'band', [{minValue, Min}, {maxValue, Max}]}]), Req, State};

send_array(Req, #network{}, DevAddr, ActRec, #state{format=qgraph}=State) ->
    Array = [{cols, [
                [{id, <<"fcnt">>}, {label, <<"FCnt">>}, {type, <<"number">>}],
                [{id, <<"average_rssi">>}, {label, <<"Average RSSI (dBm)">>}, {type, <<"number">>}],
                [{id, <<"rssi">>}, {label, <<"RSSI (dBm)">>}, {type, <<"number">>}],
                [{id, <<"average_snr">>}, {label, <<"Average SNR (dB)">>}, {type, <<"number">>}],
                [{id, <<"snr">>}, {label, <<"SNR (dB)">>}, {type, <<"number">>}]
                ]},
            {rows, lists:map(
                fun(#rxframe{fcnt=FCnt, gateways=Gateways, average_qs=AverageQs}) ->
                    {_MAC, #rxq{rssi=RSSI, lsnr=SNR}} = hd(Gateways),
                    {AvgRSSI, AvgSNR} =
                        if
                            is_tuple(AverageQs) -> AverageQs;
                            AverageQs == undefined -> {null, null}
                        end,
                    [{c, [
                        [{v, FCnt}],
                        [{v, AvgRSSI}],
                        [{v, RSSI}],
                        [{v, AvgSNR}],
                        [{v, SNR}]
                    ]}];
                (_Else) ->
                    [{c, []}]
                end, ActRec)
            }
        ],
    {jsx:encode([{devaddr, DevAddr}, {array, Array}]), Req, State}.

resource_exists(Req, State) ->
    case mnesia:dirty_read(node,
            lorawan_admin:parse_field(devaddr, cowboy_req:binding(devaddr, Req))) of
        [] -> {false, Req, State};
        [_Node] -> {true, Req, State}
    end.

% end of file
