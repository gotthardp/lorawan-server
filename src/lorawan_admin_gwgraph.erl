%
% Copyright (c) 2016-2017 Petr Gotthard <petr.gotthard@centrum.cz>
% All rights reserved.
% Distributed under the terms of the MIT License. See the LICENSE file.
%
-module(lorawan_admin_gwgraph).

-export([init/2]).
-export([is_authorized/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([get_gateway/2]).

-include_lib("lorawan_server_api/include/lorawan_application.hrl").
-include("lorawan.hrl").
-record(state, {format}).

init(Req, [Format]) ->
    {cowboy_rest, Req, #state{format=Format}}.

is_authorized(Req, State) ->
    lorawan_admin:handle_authorization(Req, State).

allowed_methods(Req, State) ->
    {[<<"OPTIONS">>, <<"GET">>], Req, State}.

content_types_provided(Req, State) ->
    {[
        {{<<"application">>, <<"json">>, []}, get_gateway}
    ], Req, State}.

get_gateway(Req, #state{format=pgraph}=State) ->
    MAC = cowboy_req:binding(mac, Req),
    Delays =
        case mnesia:dirty_read(gateways, lorawan_mac:hex_to_binary(MAC)) of
            [#gateway{delays=D}] when D /= undefined -> D;
            _Else -> []
        end,
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    Array = [{cols, [
                [{id, <<"timestamp">>}, {label, <<"Timestamp">>}, {type, <<"datetime">>}],
                [{id, <<"srvdelay">>}, {label, <<"Server Delay [ms]">>}, {type, <<"number">>}],
                [{id, <<"nwkdelay">>}, {label, <<"Network Delay [ms]">>}, {type, <<"number">>}]
                ]},
            {rows, lists:filtermap(
                fun ({Date, SDelay, NDelay}) ->
                    {true,  [{c, [
                                [{v, encode_timestamp(Date)}],
                                [{v, SDelay}],
                                [{v, NDelay}]
                            ]}]};
                (_Else) ->
                    false
                end, Delays)
            }
        ],
    {jsx:encode([{mac, MAC}, {array, Array}]), Req, State};

get_gateway(Req, #state{format=tgraph}=State) ->
    MAC = cowboy_req:binding(mac, Req),
    Dwell =
        case mnesia:dirty_read(gateways, lorawan_mac:hex_to_binary(MAC)) of
            [#gateway{dwell=D}] when D /= undefined -> D;
            _Else -> []
        end,
    % construct Google Chart DataTable
    % see https://developers.google.com/chart/interactive/docs/reference#dataparam
    Array = [{cols, [
                [{id, <<"timestamp">>}, {label, <<"Timestamp">>}, {type, <<"datetime">>}],
                [{id, <<"dwell">>}, {label, <<"Tx Time [ms]">>}, {type, <<"number">>}],
                [{id, <<"sum">>}, {label, <<"Tx in Hour [ms]">>}, {type, <<"number">>}]
                ]},
            {rows, lists:filtermap(
                fun ({Date, {_, Duration, Sum}}) ->
                    {true,  [{c, [
                                [{v, encode_timestamp(Date)}],
                                [{v, round(Duration)}],
                                [{v, Sum/36000}, {f, <<(integer_to_binary(round(Sum)))/binary,
                                    " (", (float_to_binary(Sum/36000, [{decimals, 3}, compact]))/binary, "%)">>}]
                            ]}]};
                (_Else) ->
                    false
                end, Dwell)
            }
        ],
    {jsx:encode([{mac, MAC}, {array, Array}]), Req, State}.


encode_timestamp({{Yr,Mh,Dy},{Hr,Me,Sc}}) ->
    list_to_binary(
        lists:concat(["Date(",
            integer_to_list(Yr), ",", integer_to_list(Mh), ",", integer_to_list(Dy), ",",
            integer_to_list(Hr), ",", integer_to_list(Me), ",", integer_to_list(trunc(Sc)), ")"])).

resource_exists(Req, State) ->
    case mnesia:dirty_read(gateways,
            lorawan_mac:hex_to_binary(cowboy_req:binding(mac, Req))) of
        [] -> {false, Req, State};
        [_Gateway] -> {true, Req, State}
    end.

% end of file
