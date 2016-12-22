%
% Copyright (c) 2016 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_rx).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_rxframe/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").

% range encompassing all possible LoRaWAN bands
-define(DEFAULT_RANGE, {433, 928}).

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_rxframe}
    ], Req, State}.

get_rxframe(Req, State) ->
    DevAddr = cowboy_req:binding(devaddr, Req),
    {_, ActRec} = lorawan_db:get_rxframes(lorawan_mac:hex_to_binary(DevAddr)),
    % guess which frequency band the device is using
    {Min, Max} = case ActRec of
        [#rxframe{freq=Freq} | _] ->
            find_range(Freq, [{433, 435}, {779, 787}, {863, 870}, {902, 928}]);
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
            {rows, lists:map(fun(#rxframe{fcnt=FCnt, datr=DatR, freq=Freq}) ->
                    [{c, [
                        [{v, FCnt}],
                        [{v, datr_to_num(DatR)}, {f, DatR}],
                        [{v, Freq}]
                    ]}]
                end, ActRec)
            }
        ],
    {jsx:encode([{devaddr, DevAddr}, {array, Array}, {'band', [{minValue, Min}, {maxValue, Max}]}]), Req, State}.

resource_exists(Req, State) ->
    case mnesia:dirty_index_read(rxframes,
            lorawan_mac:hex_to_binary(cowboy_req:binding(devaddr, Req)), #rxframe.devaddr) of
        [] -> {false, Req, State};
        [_First|_Rest] -> {true, Req, State}
    end.

find_range(Freq, [{Min, Max}|_Res]) when Freq >= Min, Freq =< Max -> {Min, Max};
% we assume the Min-Max tuples are ordered
find_range(Freq, [{_Min, Max}|_Rest]) when Freq < Max -> ?DEFAULT_RANGE;
find_range(Freq, [_MinMax|Rest]) -> find_range(Freq, Rest);
find_range(_Freq, []) -> ?DEFAULT_RANGE.

datr_to_num(Config) ->
    [SF, BW] = binary:split(Config, [<<"SF">>, <<"BW">>], [global, trim_all]),
    case {binary_to_integer(SF), binary_to_integer(BW)} of
        {12, 125} -> 0;
        {11, 125} -> 1;
        {10, 125} -> 2;
        {9, 125} -> 3;
        {8, 125} -> 4;
        {7, 125} -> 5;
        {7, 250} -> 6
    end.

% end of file
