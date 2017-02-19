%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_rxgraph).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_rxframe/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").
-record(state, {format}).

% range encompassing all possible LoRaWAN bands
-define(DEFAULT_RANGE, {433, 928}).

init(Req, [Format]) ->
    {cowboy_rest, Req, #state{format=Format}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_rxframe}
    ], Req, State}.

get_rxframe(Req, #state{format=rgraph}=State) ->
    DevAddr = cowboy_req:binding(devaddr, Req),
    {_, ActRec} = lorawan_db:get_rxframes(lorawan_mac:hex_to_binary(DevAddr)),
    % guess which frequency band the device is using
    {Min, Max} = case ActRec of
        [#rxframe{region=Region} | _] ->
            lorawan_mac_region:freq_range(Region);
        [] ->
            ?DEFAULT_RANGE
    end,
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    Array = [{cols, [
                [{id, <<"fcnt">>}, {label, <<"FCnt">>}, {type, <<"number">>}],
                [{id, <<"datr">>}, {label, <<"Data Rate">>}, {type, <<"number">>}],
                [{id, <<"freq">>}, {label, <<"Frequency (MHz)">>}, {type, <<"number">>}]
                ]},
            {rows, lists:map(fun(#rxframe{fcnt=FCnt, region=Region, datr=DatR, freq=Freq}) ->
                    [{c, [
                        [{v, FCnt}],
                        [{v, lorawan_mac_region:datar_to_dr(Region, DatR)}, {f, DatR}],
                        [{v, Freq}]
                    ]}]
                end, ActRec)
            }
        ],
    {jsx:encode([{devaddr, DevAddr}, {array, Array}, {'band', [{minValue, Min}, {maxValue, Max}]}]), Req, State};

get_rxframe(Req, #state{format=qgraph}=State) ->
    DevAddr = cowboy_req:binding(devaddr, Req),
    {_, ActRec} = lorawan_db:get_rxframes(lorawan_mac:hex_to_binary(DevAddr)),
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    Array = [{cols, [
                [{id, <<"fcnt">>}, {label, <<"FCnt">>}, {type, <<"number">>}],
                [{id, <<"rssi">>}, {label, <<"RSSI (dBm)">>}, {type, <<"number">>}],
                [{id, <<"snr">>}, {label, <<"SNR (dB)">>}, {type, <<"number">>}]
                ]},
            {rows, lists:map(fun(#rxframe{fcnt=FCnt, rssi=RSSI, lsnr=SNR}) ->
                    [{c, [
                        [{v, FCnt}],
                        [{v, RSSI}],
                        [{v, SNR}]
                    ]}]
                end, ActRec)
            }
        ],
    {jsx:encode([{devaddr, DevAddr}, {array, Array}]), Req, State}.

resource_exists(Req, State) ->
    case mnesia:dirty_index_read(rxframes,
            lorawan_mac:hex_to_binary(cowboy_req:binding(devaddr, Req)), #rxframe.devaddr) of
        [] -> {false, Req, State};
        [_First|_Rest] -> {true, Req, State}
    end.

% end of file
